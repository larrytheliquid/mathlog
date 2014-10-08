{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main  where

import SimpleProp

import Test.QuickCheck hiding(Prop(..))
import Test.HUnit

-- cabal install SmallCheck
import qualified LazySmallCheck as SC
import LazySmallCheck
--import Test.SmallCheck.Series
import Control.Monad(liftM,liftM2)
   
--------------------------------------------
-- from the Coble notes page 13

-- 1 level equivalences as transformations

implies1 x y = OrP (not1 x) y

not1 TruthP = AbsurdP
not1 AbsurdP = TruthP
not1 (NotP x) = x
not1 (AndP x y) = OrP(not1 x) (not1 y)
not1 (OrP x y) = AndP (not1 x) (not1 y)
not1 (ImpliesP x y) = AndP x (not1 y)
not1 x = NotP x

-- A top down use

elimImplies x =
  case x of
    AbsurdP -> AbsurdP
    TruthP  -> TruthP
    (LetterP x) -> LetterP x
    (AndP x y) -> AndP (elimImplies x) (elimImplies y)
    (OrP x y) -> OrP (elimImplies x) (elimImplies y)
    (ImpliesP x y) -> elimImplies(implies1 x y)
    (NotP x) -> NotP (elimImplies x)

-- A bottom up use
    
nnf x =
  case x of 
    AbsurdP -> AbsurdP
    TruthP  -> TruthP
    (LetterP x) -> LetterP x
    (AndP x y) -> AndP (nnf x) (nnf y)
    (OrP x y) -> OrP (nnf x) (nnf y)
    (ImpliesP x y) -> nnf(OrP (NotP x) y)
    (NotP x) -> not1(nnf x)


    
-- assumes already in negation-normal-form    
pushDisj x =
  case x of
    OrP x y -> case (pushDisj x,pushDisj y) of
      (AndP a b,AndP c d) -> 
         AndP (pushDisj (OrP a c))
              (AndP (pushDisj (OrP a d))
                    (AndP (pushDisj (OrP b c))
                    (pushDisj (OrP b d))))
      (a,AndP b c) -> 
         AndP (pushDisj (OrP a b)) 
              (pushDisj (OrP a c))                   
      (AndP b c,a) -> 
         AndP (pushDisj (OrP b a))
              (pushDisj (OrP c a)) 
      (x,y) -> OrP x y
    AbsurdP -> AbsurdP
    TruthP  -> TruthP
    (LetterP x) -> LetterP x
    (AndP x y) -> AndP (pushDisj x) (pushDisj y)
    (ImpliesP x y) -> pushDisj(OrP (NotP x) y)
    (NotP x) -> NotP (pushDisj x)

-- assumes all disjunctions have been pushed inside
-- so only literals appear inside OrP
flatten:: Prop n -> [[Prop n]]
flatten (AndP x y) = flatten x ++ flatten y
flatten (OrP x y) = [collect [x,y]]
  where collect [] = []
        collect (OrP x y : zs) = collect (x:y:zs)
        collect (z:zs) = z : collect zs
flatten x = [[x]]        

simple:: Eq n => [[Prop n]] -> [[Prop n]]
simple [] = []
simple (x:xs) 
  | elem TruthP x = simple xs
  | conjugatePair x = simple xs
  | subsumes xs x = simple xs
  | otherwise = x : simple xs

conjugatePair [] = False
conjugatePair [x] = False
conjugatePair (LetterP x : xs) = elem (NotP (LetterP x)) xs
conjugatePair (NotP x : xs) = elem x xs
conjugatePair (x:xs) = conjugatePair xs

subsumes [] xs = False
subsumes (ys : yss) xs = all ( `elem` xs) ys || subsumes yss xs 

cnf3 :: Eq n => Prop n -> [[Prop n]]
cnf3 x = (simple . flatten .  pushDisj . nnf . elimImplies) x

cnf4 :: Prop t -> Prop t
cnf4 = pushDisj . nnf . elimImplies
    
   

t = TruthP
f = AbsurdP

---------------------------------------
-- A principled approach
-- Study the equivalence
-- (A /\ B) \/ (C /\ D) == (A \/ C) /\ (A \/ D) /\ (B \/ C) /\ (B \/ D)
-- [A,B] \/ [C,D] ==  /\ [ x \/ y | x <- [A,B], y <- [C,D]]
-- Take as input a list of disjunctions, and apply the
-- cross product rule

cnf2 :: Prop Int -> [[Prop Int]]
cnf2 TruthP = []
cnf2 AbsurdP = [[]]
cnf2 p = process [p]
process [] = [[]]
process (p:ps) = 
  case p of
   (AbsurdP)             -> map (AbsurdP:) (process ps)
   (TruthP)              -> map (TruthP:) (process ps)
   (LetterP _)           -> map (p:) (process ps)
   (AndP x y)            -> process (x:ps) ++ process (y:ps)
   (OrP x y)             -> process (x : y : ps)
   (ImpliesP x y)        -> process (NotP x : y : ps)  
   (NotP z) -> 
       case z of 
          (AbsurdP)      -> map (TruthP:) (process ps)
          (TruthP)       -> map (AbsurdP:) (process ps)
          (LetterP _)    -> map (p:) (process ps)
          (AndP x y)     -> process (NotP x : NotP y : ps)
          (OrP x y)      -> process (NotP x:ps) ++ process (NotP y:ps)
          (ImpliesP x y) -> process (x:ps) ++ process (NotP y:ps)
          (NotP p2)      -> process (p2:ps)  


-----------------------------------------------------
-----------------------------------------------------------------
-- CNF and DNF
-- set up some machinery for uniform notation

alpha :: Prop a -> Maybe (Prop a, Prop a) 
alpha (AndP x y) = return (x,y)
alpha (NotP (OrP x y)) = return (NotP x, NotP y)
alpha (NotP (ImpliesP x y)) = return (x, NotP y)
alpha _ = Nothing
       
beta :: Prop a -> Maybe (Prop a, Prop a)
beta (NotP (AndP x y)) = return (NotP x, NotP y)
beta (OrP x y) = return (x,y)
beta (ImpliesP x y) = return (NotP x, y)
beta _ = Nothing

isLit (NotP (LetterP _)) = True
isLit (LetterP _) = True
isLit AbsurdP = True
isLit TruthP = True
isLit _ = False

cnf :: Prop a -> [[Prop a]]
cnf TruthP = []
cnf AbsurdP = [[]]
cnf p = processClause alpha beta [p]

dnf AbsurdP = []
dnf TruthP = [[]]
dnf p = processClause beta alpha [p]

processClause :: (Prop a -> Maybe (Prop a, Prop a)) -> 
                   (Prop a -> Maybe (Prop a, Prop a)) ->
                   [Prop a] -> [[Prop a]]
processClause alphy bety [] = [[]]
processClause alphy bety (p : ps) 
  | isLit p = map (p:) $ processClause alphy bety ps
  | otherwise = 
    case p of
      NotP TruthP -> map (AbsurdP:) $ processClause alphy bety ps
      NotP AbsurdP -> map (TruthP:) $ processClause alphy bety ps
      NotP (NotP p') -> processClause alphy bety (p' : ps)
      otherwise -> 
        case bety p of
          Just (x,y) -> processClause alphy bety (x : y : ps)
          Nothing -> 
            case alphy p of
              Just (x,y) -> processClause alphy bety (x : ps) ++ processClause alphy bety (y : ps)
              Nothing -> error "IMPOSSIBLE IN PROCESS CLAUSE"

------------------------------------------------------
-- Elegant method using discriminators
-- the discrim function will be reusable elsewhere.

data Discrim a = Alpha a a | Beta a a | Lit a

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))


processCl :: [Prop a] -> [[Prop a]]
processCl [] = [[]]
processCl (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (processCl ps)
    Alpha x y -> processCl (x : ps) ++ processCl (y : ps)
    Beta x y -> processCl (x : y : ps)
    
cnf5 x = processCl [x]  

cnf6 x = foldr AndP TruthP (map (foldr OrP AbsurdP) (cnf5 x))
  
----------------------------------------------------
-- some checking code

instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]
newtypeCons x = cons1 x

instance Serial a => Serial (Prop a) where
 series = newtypeCons LetterP SC.\/ cons1 NotP SC.\/ cons2 OrP SC.\/
          cons2 AndP  SC.\/ cons0 AbsurdP SC.\/ cons0 TruthP SC.\/
          cons2 ImpliesP


instance Serial Name where
  series = cons0 A SC.\/ cons0 B  -- SC.\/ cons0 C
  
instance Arbitrary Name where
  -- coarbitrary = undefined
  arbitrary = oneof [return A, return B, return C]


assigns:: [a] -> [[(a,Bool)]]
assigns [] = []
assigns [n] = [[(n,True)],[(n,False)]]
assigns (x:xs) = map ((x,True):) ys ++ map ((x,False):) ys
  where ys = assigns xs  
  
envs = assigns [A,B] -- ,C]

eval env x = value (liftE env) x 

liftE pairs x = case lookup x pairs of
                 Just b -> b
                 other -> False

-- returns a Bool                 
check x = and[ (eval e x == eval e (cnf6 x)) | e <- envs] 

-- returns a LazySmallCheck property
check2 x = foldl (*&*) (lift True) [ lift(eval e x == eval e (cnf6 x)) | e <- envs]

t1 = quickCheck check
t2 = smallCheck 1 check2

main = t2