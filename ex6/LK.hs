module LK where

import Formula
import Tactics
import Print
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)
import qualified Text.PrettyPrint.HughesPJ as PP
import Subst
import Parser(toFormula,toTerm)
import Control.Monad
import Term
import Control.Monad.State


-- we generalize slightly and turn our notion of a 
-- tactic from SequentM -> Maybe [SequentM] to
-- SequentM -> Proof [SequentM]
-- CCH

type Proof = StateT ([Int],[SequentM]) Maybe
type TermS = Term String String

onAll f (SequentM hs gs) = SequentM (map f hs) (map f gs)

-- inc :: Proof Int
inc = do
  (ns,gs) <- get
  put $ (tail ns, gs)
  return $ head ns

type FormulaS = Formula String String String

data SequentM = SequentM [FormulaS] [FormulaS]

instance PP SequentM where  
  pp(SequentM hyps concl) = 
    PP.sep[ PP.sep (PP.punctuate (text ",") (map pp hyps))
          , text "|-"
          , PP.sep (PP.punctuate (text ",") (map pp concl)) ]

instance Show SequentM where
  show x = render(pp x)
    
splice old n new = (take (n-1) old) ++ new ++ (drop n old)

-- This is 1 (rather than 0) based and fails if there
-- aren't enough items in the list 

nth:: MonadPlus m => [a] -> Int -> m a
nth _ 0 = mzero
nth [] _ = mzero
nth (x:_) 1 = return x
nth (_:xs) (m) = nth xs (m-1)

-----------------------------------------------------
-- individual Sequent rules

andL :: MonadPlus m => Int -> SequentM -> m [SequentM]
andL n (SequentM hs gs) = do
  Conn And [p,q] <- nth hs n
  return [SequentM (splice hs n [p,q]) gs]

andR n (SequentM hs gs) = do 
  Conn And [p,q] <- nth gs n
  return [SequentM hs (splice gs n [p]),
          SequentM hs (splice gs n [q])]

orL n (SequentM hs gs) = do 
  Conn Or [p,q] <- nth hs n
  return [SequentM (splice hs n [p]) gs,
          SequentM (splice hs n [q]) gs]

orR n (SequentM hs gs) = 
   do Conn Or [p,q] <- nth gs n
      return [SequentM hs (splice gs n [p,q])]

impliesL n (SequentM hs gs) = 
   do Conn Imp [p,q] <- nth hs n
      return [SequentM (splice hs n []) (p:gs),
              SequentM (splice hs n [q]) gs]

impliesR n (SequentM hs gs) = 
   do Conn Imp [p,q] <- nth gs n
      return [SequentM (p:hs) (splice gs n [q])]

notL n (SequentM hs gs) = 
   do Conn Not [p] <- nth hs n
      return [SequentM (splice hs n []) (p:gs)]

notR n (SequentM hs gs) = 
   do Conn Not [p] <- nth gs n
      return [SequentM (p:hs) (splice gs n [])]

trueR n (SequentM hs gs) =
  do Conn T [] <- nth gs n
     return []

falseL n (SequentM hs gs) = 
  do Conn F [] <- nth hs n
     return []

exL n (SequentM hs gs) = 
  do Quant Exist x a <- nth hs n
     newName <- newVar `fmap` inc
     let hs' = (subst (x |-> newName) a) : hs
     return [SequentM hs' gs]

exR f n (SequentM hs gs) = do
  Quant Exist x a <- nth gs n
  let gs' = splice gs n [(subst (x |-> f) a)]
  return [SequentM hs gs']

-- allL :: TermS -> Int -> SequentM -> Proof [SequentM]
allL f n (SequentM hs gs) = do
  Quant All x a <- nth hs n
  let hs' = splice hs n [(subst (x |-> f) a)]
  return [SequentM hs' gs]
  
-- allR :: MonadPlus m => Int -> SequentM -> m[SequentM]
allR n (SequentM hs gs) = 
    do Quant All x a <- nth gs n
       newName <- newVar `fmap` inc
       let gs' = (subst (x |-> newName) a) : gs
       return [SequentM hs gs']
  
-- axiom :: Int -> Int -> SequentM -> Proof [SequentM]
axiom i j (SequentM hs gs) =
    do phi <- nth hs i
       psi <- nth gs j
       guard (phi == psi)
       return []


-- Structural Rules

weakening i (SequentM hs gs) = return [SequentM hs (splice gs i [])]

contraction i (SequentM hs gs) = do 
  phi <- nth hs i
  return $ SequentM (splice hs i [phi,phi]) gs

cut :: FormulaS -> SequentM -> Proof [SequentM]
cut p (SequentM hs gs) = return [SequentM (p:hs) gs,
                                 SequentM hs (p:gs)] 


-------------------------------------------------------
-- helper functions to interact with the Proof monad

putGoals:: [SequentM] -> Proof ()       
putGoals gs = do
  (ns,gs') <- get
  put $ (ns,gs' ++ gs)

getGoal :: Proof SequentM
getGoal = do
  g <- gets (head . snd)
  modify (\(a,b) -> (a,tail b)) 
  return g
  
---------------------------------------------------------------
-- Running a proof

runProof' :: [SequentM -> Proof [SequentM]] -> Proof [SequentM]
runProof' [] = gets snd 
runProof' (t : ts) = do
  cgoal <- getGoal
  newGoals <- t cgoal
  putGoals newGoals
  runProof' ts

runProof :: SequentM -> [SequentM -> Proof [SequentM]] -> Maybe [SequentM]
runProof s ts = evalStateT (runProof' ts) ([1..],[s])


proveM s ts = case runProof s ts of
                Just ss -> pp ss
                Nothing -> text "Proof fails"

---------------------------------------------------------
-- Sometimes its better to keep a record of what happened
-- and to print it out. Very similar in control flow to
-- runProof, but builds a pretty printed document.

replayM s rs =
   do zs <- replayStepsM [] [s] rs 
      return(docify $ reverse $ zs)

replayStepsM script [] [] = return((QED, Just []):script)
replayStepsM script ss [] = return((Open, Just ss):script)
replayStepsM script (s:ss) (r:rs) =
    mplus (do ss' <- r s
              replayStepsM ((Focus s,Just ss'):script) (ss'++ss) rs)
          (return((Focus s,Nothing):script))
          
          
--------------------------------------------------------------
-- Here we make some rules which try many possibilities
-- rathen than asking the user to choose one.

tryL r (SequentM hs gs) = (oneOf $ map (\i -> r i) [1..length hs]) (SequentM hs gs)
tryR r (SequentM hs gs) = (oneOf $ map (\i -> r i) [1..length gs]) (SequentM hs gs)
tryLR r = tryL (\i -> tryR (\j -> r i j))
aL, aR, oL, oR, iL, iR, nL, nR, tR, fL, alR, eL :: SequentM -> Proof [SequentM]
aL = tryL andL
aR = tryR andR
oL = tryL orL
oR = tryR orR
iL = tryL impliesL
iR = tryR impliesR
nL = tryL notL
nR = tryR notR
tR = tryR trueR
fL = tryL falseL

alR = tryR allR

eL = tryL exL

eR, alL :: Term String String -> SequentM -> Proof [SequentM]
eR t = tryL (exR t)
alL t = tryR (allL t)


ax x = tryLR axiom x

newParam n = newFun n []


------------------------------------------------------------------
          
f1 = toFormula "(forall x. P(x)) --> (forall y. P(F(y)))"
s1 = (SequentM [] [f1])

m1:: Proof Doc
m1 = (replayM s1 [impliesR 1,allR 1,allL (toTerm "F(?b)") 1,axiom 1 1])
ex1 = evalStateT m1 ([1..],[s1])
          
          
