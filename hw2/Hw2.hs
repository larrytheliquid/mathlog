-- Larry Diehl
{-# LANGUAGE  DeriveDataTypeable   #-}

module Hw2 where

import Prop
import FiniteSet
import Data.Typeable

----------------------------------------------------------------------

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show,Read,Enum,Eq,Ord,Typeable)

d1 = dimS ["Odd", "Even"]
d2 = dim 10
d3 = Dim (length r) Int [LInt x | x <- r]
  where r = [5..10]
d4 = dimE "Day" Mon

----------------------------------------------------------------------