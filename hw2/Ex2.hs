{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main  where

import SimpleProp

import Test.QuickCheck hiding(Prop(..))
import Test.HUnit

import qualified LazySmallCheck as SC
import LazySmallCheck
import Control.Monad(liftM,liftM2)

t = TruthP
f = AbsurdP

----------------------------------------------------------------------

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

cnf' :: Eq n => Prop n -> [[Prop n]]
cnf' x = (simple . flatten .  pushDisj . nnf . elimImplies) x

----------------------------------------------------------------------

toOrs :: [Prop n] -> Prop n
toOrs [] = f
toOrs [x] = x
toOrs (x:xs) = OrP x (toOrs xs)

toAnds :: [Prop n] -> Prop n
toAnds [] = t
toAnds [x] = x
toAnds (x:xs) = AndP x (toAnds xs)

toProp :: Eq n => [[Prop n]] -> Prop n
toProp = toAnds . map toOrs

cnf :: Eq n => Prop n -> Prop n
cnf = toProp . cnf'
  
----------------------------------------------------------------------

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

----------------------------------------------------------------------

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
check x = and[ (eval e x == eval e (cnf x)) | e <- envs] 

-- returns a LazySmallCheck property
check2 x = foldl (*&*) (lift True) [ lift(eval e x == eval e (cnf x)) | e <- envs]

t1 = quickCheck check
t2 = smallCheck 2 check2

main = t2

----------------------------------------------------------------------