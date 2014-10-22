{-# LANGUAGE  DeriveDataTypeable   #-}

module Soduko where

import FiniteSet
import Prop
import Minisat


{-   0   1   2   3
   +---+---+---+---+
0  |   |   |   | 4 |   (0,3,4)
   +---+---+---+---+
1  |   | 2 | 1 |   |   (1,1,2) (1,2,1)
   +---+---+---+---+
2  |   | 1 | 4 |   |   (2,1,1) (2,2,4)
   +---+---+---+---+
3  |   |   |   | 1 |   (3,3,1)
   +---+---+---+---+
-}

d1 = dim 4
d2 = dimS ["1","2","3","4"]

(_,grid1) = enum f [d1,d1,d2] 1
  where f [a,b,c] i = Just(i+1,LetterP i)

grid2 :: FiniteSet (Prop Int)        
grid2 = fromFList [d1,d1,d2] (map (\ x -> (x,TruthP)) 
                      [(0::Int,3::Int,"4"),(1,1,"2"),(1,2,"1"),(2,1,"1"),(2,2,"4"),(3,3,"1")])
                      
grid = union grid1  grid2

row i = full (project [2] (select (\ [a,b,c] -> a==i) grid))
col i = full (project [2] (select (\ [a,b,c] -> b==i) grid))

box 0 = select (\ [a,b,c] -> a < 2 && b <2) grid
box 1 = select (\ [a,b,c] -> elem a [0,1] && elem b [2,3]) grid
box 2 = select (\ [a,b,c] -> elem a [2,3] && elem b [0,1]) grid
box 3 = select (\ [a,b,c] -> a >= 2 && b >= 2) grid

quad i = full (project [2] (box i))

form =  andL ([andL [ row i, col i, quad i] | i <- [0..3]] ++
              [funDep [0,1] [2] grid])
              
ww = cnf form
file = putClauses "soduko.cnf" ww
              