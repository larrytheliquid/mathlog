module NatDed where

import Control.Monad(liftM)
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($+$),render)

import SimpleProp
import Blocks

-- In this file we define a datatype that represents Natural Deduction
-- style proofs. We write two functions
-- 1) check, this checks that a NatDed data represenst a valid proof
-- 2) split, this breaks a NatDed data into 3 parts
--      a) A list of blocks representing (a printable) set of hypothesis (above the line)
--      b) A (Prop n) representing what the NatDed data proves
--      c) The name of the top most rule in the proof
--
-- We then use these to make (NatDed n) an instance of the Show class
-- Thus when you evaluate a (NatDed n) the proof, or an error, is printed.

data NatDed n 
  = Premise (Prop n)
  | AndI (NatDed n) (NatDed n)
  | AndE1 (NatDed n)
  | AndE2 (NatDed n)
  | Neg2I (NatDed n)
  | Neg2E (NatDed n)
  | ImplyI (Prop n) (NatDed n)
  | ImplyE (NatDed n) (NatDed n)
  | OrI1 (NatDed n) (Prop n)
  | OrI2 (Prop n) (NatDed n)
  | OrE (NatDed n) (NatDed n) (NatDed n)
  | AbsurdE (NatDed n) (Prop n)
  | AbsurdI (NatDed n) (NatDed n)
  | NegI (Prop n) (NatDed n)

data Sequent n = Seq [Prop n] (NatDed n)

badCase = ([oneBlock "??"],AbsurdP,"")

split :: (Eq n,PPLetter n) => [Prop n] -> NatDed n -> ([Block],Prop n,String)
split hyps (Premise TruthP) = ([],TruthP,"")
split hyps (Premise AbsurdP) = ([],AbsurdP,"")
split hyps (Premise p) | elem p hyps = ([],p,"")
split hyps (Premise p) = ([oneBlock ("?{"++show p++"}")],p,"")
split hyps (AndI x y) = ([draw t1,draw t2],AndP a b," /\\i") 
  where t1@(b1,a,n1) = split hyps x; t2@(b2,b,n2) = split hyps y
split hyps (AndE1 p) =  
  case split hyps p of
    (t1@(bs,AndP x y,_)) -> ([draw t1],x," /\\e1")
    (_,other,_) -> ([oneBlock "??"],AbsurdP, " /\\e1")
split hyps (AndE2 p) =  
  case split hyps p of
    (t1@(bs,AndP x y,_)) -> ([draw t1],y," /\\e2")
    (_,other,_) -> ([oneBlock "??"],AbsurdP, " /\\e2")
split hyps (Neg2I x) = ([draw t1],NotP(NotP p)," ~~i")
  where t1@(bs,p,nm) = split hyps x
split hyps (Neg2E x) = 
  case split hyps x of
    t1@(bs,q@(NotP(NotP x)),nm) -> ([draw t1],x," ~~e")
    (_,other,_) -> badCase
split hyps (ImplyI x y) = ([cap 0 (draw t1)],ImpliesP x z," =>i")
  where t1@(bs,z,nm) = split (x : hyps) y
split hyps (ImplyE x y) = 
  case (split hyps x, split hyps y) of
    (t1@(b1,a1,n1),t2@(b2,ImpliesP a2 b,n2)) ->
      if a1==a2
         then ([draw t1,draw t2],b," ->e")
         else badCase
    (t1@(b1,a1,n1),t2@(b2,a2,n2)) -> badCase
split hyps (OrI1 p x) = ([draw t1],OrP y x," \\/i1")
  where t1@(bs,y,name) = split hyps p
split hyps (OrI2 x p) = ([draw t1],OrP x y," \\/i2")
  where t1@(bs,y,name) = split hyps p  
split hyps (OrE p q r) =
  case (split hyps p) of
     t1@(bs,OrP a b,nm) ->
          if x1==x2
             then ([draw t1,cap 0 (draw t2),cap 0 (draw t3)],x1," \\/e")
             else badCase
       where t2@(bs1,x1,n1) = split (a:hyps) q
             t3@(bs2,x2,n2) = split (b:hyps) r   
split hyps (AbsurdE x p) =
  case split hyps x of
    t1@(bs,AbsurdP,nm) -> ([draw t1],p," Fe")
    t2 -> ([oneBlock "?{not F}"],p," Fe")
split hyps (AbsurdI x y) = 
  let t1@(_,a,_) = split hyps x
  in case split hyps y of
       t2@(_,NotP w,_) | a==w -> ([draw t1,draw t2],AbsurdP," Fi")
       t2@(_,NotP w,_) -> ([draw t1,oneBlock "{Not ?}"],AbsurdP," Fi")
       other -> ([draw t1,beside(oneBlock "?")(draw other)],AbsurdP," Fi")
split hyps (NegI p x) =
  case split (p : hyps) x of
    t1@(_,AbsurdP,_) -> ([draw t1],NotP p," ~i")
    (bs,x,nm) -> ([sequentBlock bs (oneBlock ("?"++show x)) nm],NotP p," ~i")

-- Once we split a (NatDed n) proof, we can draw it by using the
-- three parts. The [Block] goes above the line, the (Prop n)
-- goes below the line, and the (String) names the rule.
-- It is an invariant that when spliting a term only Premises
-- have an empty list of hypothesis.

draw:: PPLetter n => ([Block],Prop n,String) -> Block        
draw ([],form@TruthP,name) = to form
draw ([],form@AbsurdP,name) = to form
draw ([],form,name) = bracket (to form)
  where -- An empty list of hypothesese means this must be a Premise
        -- draw backets around it to show it should be "discharged"
        bracket form = lineUp[oneBlock "[", form, oneBlock "]"]
draw (xs,x,name) = schema hyps (center (to x) width) name
  where hyps = (sep 3 xs)
        (width,l) = dim hyps

-- To check a (NatDed n) proof there are a few rules that 
--its structure shopuld have. Test these rules, if one doesn't
-- hold, return the Left part of the Either with a error message.

lift2 f (Right x) (Right y) = Right(f x y)
lift2 f (Left s) (Left t) = Left(s++"\n"++t)
lift2 f (Left s) _ = Left s
lift2 f _ (Left s) = Left s

check:: (Eq n,PPLetter n) => [Prop n] -> NatDed n -> Either String (Prop n)
check hyps (Premise TruthP) = return TruthP
-- Note that (Premise AbsurdP) checks only if it is assumed.
check hyps (Premise p) | elem p hyps = return p
check hyps (Premise p) = Left("The formula "++show p++" is not amongst the premises: "++show hyps)
check hyps (AndI x y) = lift2 AndP (check hyps x) (check hyps y) 
check hyps (AndE1 p) =  
  case check hyps p of
    (Right(AndP x y)) -> return x
    (Left s) -> Left s
    (Right other) -> Left ("AndE1 applied to non AndP: "++show other)
check hyps (AndE2 p) =  
  case check hyps p of
    (Right(AndP x y)) -> return y
    (Left s) -> Left s
    (Right other) -> Left ("AndE2 applied to non AndP: "++show other)
check hyps (Neg2I x) =  liftM (NotP . NotP) (check hyps x)
check hyps (Neg2E x) = 
  case check hyps x of
    (Right(NotP(NotP x))) -> return x
    (Left s) -> Left s
    (Right other) -> Left ("NegE applied to non NotP(NotP z)): "++show other) 
check hyps (ImplyI x y) = liftM (ImpliesP x) (check (x:hyps) y)
check hyps (ImplyE x y) = 
  case (check hyps x, check hyps y) of
    (Right a1,Right(ImpliesP a2 b)) ->
      if a1==a2
         then return b
         else Left ("1st arg to ImplyE ("++show a1++
                     ") does not match hypothesis of 2nd argument ("++
                     show a2++")")
    (Right a1,Right a2) ->
      error ("2nd arg to ImplyE is not an implication: "++show a2) 
    (Left s,_) -> Left s
    (_,Left s) -> Left s
check hyps (OrI1 p x) = lift2 OrP (check hyps p) (return x)
check hyps (OrI2 x p) = liftM (OrP x) (check hyps p)
check hyps (OrE p q r) =
  case (check hyps p) of
     (Right(OrP a b)) ->
       do { x1 <- check (a:hyps) q
          ; x2 <- check (b:hyps) r
          ; if x1==x2
               then return x1
              else Left ("Or branches not the same: "++show x1++" =/= "++show x2)
          }
     (Left s) -> Left s
check hyps (AbsurdE x p) =
  case check hyps x of
     (Right AbsurdP) -> Right p
     (Right _) -> Left ("In AbsurdE the proof does not derive F")
     (Left s) -> Left s
check hyps (AbsurdI x y) = 
  do { a <- check hyps x
     ; b <- check hyps y
     ; case b of
        NotP w | a==w -> Right AbsurdP
        NotP w -> Left "In AbsurdI the 2nd argument is a negation, but not the negation of the 1st argument."
        other -> Left "In AbsurdI the 2nd argument is not a negation"}
check hyps (NegI p x) =
  case check (p : hyps) x of
    (Right AbsurdP) -> Right (NotP p)
    (Right x) -> Left ("In NegI conclusion: "++show x++" is not F.")
    (Left s) -> Left s
    
instance (PPLetter n,Eq n) => Blockable (NatDed n) where
  toBlock x = draw (split [] x)
              
instance (Eq n,PPLetter n) => Show (NatDed n) where
  show n = let blk = (toBlock n)
           in case check [] n of
                Right p -> show blk ++ "\nWhich Proves the formula\n   "++show p
                Left message -> show blk ++ "\n **** ERROR ****\n" ++ message

instance (Eq n,PPLetter n) => Show (Sequent n) where
  show (Seq ps x) = 
     let blk = draw (split ps x)
     in case check ps x of
                Right p -> show blk ++ 
                           "\nWhich Proves the formula\n   "++
                           plistf show "" ps "," " |- "++show p
                Left message -> show blk ++ "\n **** ERROR ****\n" ++ message

p x = Premise x

t1 = AndI (AndE2 (p (0*1))) (p 2)
bad1 = AndI (AndE2 (p 1)) (p 2)
t2 = Neg2I t1
t3 = ImplyI (3) (AndI (p 3) (p TruthP))
t4 = ImplyE (p 4) (ImplyI (4) (AndI (p 4) (p 5)))
t5 = OrI1 t1 3
t6 = OrI2 3 t2
t7 = OrE (p(1 + 2)) (OrI1 (p 2) 1) (OrI2 2 (p 1))
t8 = ImplyI (ImpliesP q r)
       (ImplyI (OrP p q) 
               (OrE (Premise (OrP p q)) 
                    (OrI1 (Premise p) r) 
                    (OrI2 p (ImplyE (Premise q) (Premise (ImpliesP q r))))))
  where p = (LetterP "p"); q = LetterP "q"; r = LetterP "r"
t9 = ImplyI AbsurdP (AbsurdE (p AbsurdP) (0 * (NotP 0))) 
t10 = ImplyI 3 (ImplyI (NotP 3) (AbsurdI (p 3) (p (NotP 3))))
t11 = NegI TruthP (AbsurdI (p TruthP) (p (NotP TruthP)))

xx = undefined

 