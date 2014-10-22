-- Larry Diehl
{-# LANGUAGE  DeriveDataTypeable   #-}

module Hw2 where

import Prop
import FiniteSet
import Data.Typeable

----------------------------------------------------------------------

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show,Read,Enum,Eq,Ord,Typeable)
instance Finite Day where

dimR r = Dim (length r) Int [LInt x | x <- r]

d1 = dimS ["Odd", "Even"]
d2 = dim 10
d3 = dimR [5..10]
d4 = dimE "Day" Mon

----------------------------------------------------------------------

s1 = partial [d1,d3] $ \ [x,y] -> f (unString (indexToLit d1 x)) (unInt (indexToLit d3 y))
  where
  f "Even" n = if even n then Just True else Nothing
  f "Odd" n = if odd n then Just True else Nothing

s2 = partial [d2,d2] $ \ [x,y] -> Just $ unInt (indexToLit d2 x) + unInt (indexToLit d2 y)

s3 = partial [dx,dy] $ \ [x,y] -> unInt (indexToLit dx x) `f` unInt (indexToLit dy y)
  where
  f m n = if succ m == n then Just True else Nothing
  dx = dimR [0..4]
  dy = dimR [1..5]

s4 = fromFiniteList True [d4,d4] [ (x,y) | x <- enumFrom Mon , y <- enumFrom Mon , x < y ]

----------------------------------------------------------------------

doubleNeg :: Boolean t => t -> t
doubleNeg = neg . neg

bothNot :: Boolean t => t -> t -> t
bothNot p q = neg p `conj` neg q

----------------------------------------------------------------------