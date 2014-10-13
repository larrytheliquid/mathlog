{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main  where

import SimpleProp

import Data.List
import Test.QuickCheck hiding(Prop(..))
import Test.HUnit

import qualified LazySmallCheck as SC
-- import LazySmallCheck
import Control.Monad(liftM,liftM2)

----------------------------------------------------------------------

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

process :: [Prop a] -> [[Prop a]]
process [] = [[]]
process (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (process ps)
    Alpha x y -> process (x : y : ps)
    Beta x y -> process (x : ps) ++ process (y : ps)

tabulate :: Prop a -> [[Prop a]]
tabulate p = process [NotP p]

----------------------------------------------------------------------

toEither :: [Prop a] -> [Either a a]
toEither [] = []
toEither (LetterP a : ps) = Left a : toEither ps
toEither (NotP (LetterP a) : ps) = Right a : toEither ps
toEither (_ : ps) = toEither ps

both :: Eq a => [Either a a] -> Maybe [Either a a]
both = flip foldl (Just []) $
  \ m x -> case m of
    Nothing -> Nothing
    Just ps -> case x of
      Left p -> if any (== Right p) ps then Nothing else Just (Left p:ps)
      Right p -> if any (== Left p) ps then Nothing else Just (Right p:ps)

contra :: Eq a => [Prop a] -> Bool
contra = maybe True (const False) . both . toEither

contras :: Eq a => [[Prop a]] -> Bool
contras = all contra

----------------------------------------------------------------------

tableau :: Eq a => Prop a -> Bool
tableau = contras . tabulate

----------------------------------------------------------------------

eg1 =  0 \/ (1 /\ 2)

eg2 = 0 \/ (1 /\ 2) ~> ((0 \/ 1) /\ (0 \/ 2))


----------------------------------------------------------------------