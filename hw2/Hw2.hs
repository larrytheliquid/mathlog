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

evens = snd $ enum f [dim 5,dim 5,dim 5] 0
  where
  f [x,y,z] n = if even x && even y && even z then Just(n+1,LetterP n) else Nothing

odds = snd $ enum f [dim 5,dim 5,dim 5] 0
  where
  f [x,y,z] n = if odd x && odd y && odd z then Just(n+1,LetterP n) else Nothing

ordered = snd $ enum f [dim 5,dim 5,dim 5] 0
  where
  f [x,y,z] n = if x <= y && y <= z then Just(n+1,LetterP n) else Nothing

same = snd $ enum f [dim 5,dim 5,dim 5] 0
  where
  f [x,y,z] n = if x == y && y == z then Just(n+1,LetterP n) else Nothing

----------------------------------------------------------------------

different = complement same
compOdds = complement odds
compEvens = complement evens

nones = intersect evens odds
sameEvens = intersect same evens
orderedOdds = intersect ordered odds

----------------------------------------------------------------------

people = ["Anita","Barbara","Caleb","Frank","George","Margareet","Tim","Walter"]

tuples = [ ("Frank","Tim"),("Tim" , "Caleb"),("Walter","Frank"), 
           ("Anita","Tim"),("Margareet","Barbara"),("Barbara","Caleb")]
                   
pd = dimS people           
p = fromFiniteList True [pd,pd] tuples

children = project [1] p
parents = project [0] p
both = intersect children parents
threeGen = join 1 p (project [1,0] p)
grandparents = project [2] threeGen

----------------------------------------------------------------------

fourGen = join 3 threeGen (join 0 p (project [1,0] p))
quaduples = project [3,2,0,1] fourGen

greatgrandparents = project [3] fourGen

ggpChild = join 1 p greatgrandparents

----------------------------------------------------------------------

step x = project [2,0] $ join 1 x (project [1,0] x)

steps x 0 = x
steps x n = step (steps x (pred n))

{-
To write the transitive closure, you would have to continue to step until you step
to an empty set. Along the way, you need to accumulate the result of each step so
the accumulator can be returned when you reach the empty set.
-}

----------------------------------------------------------------------