{-# LANGUAGE  DeriveDataTypeable   #-}

module FiniteSetNotes where

import FiniteSet
import Data.Typeable
import Prop(Prop(..),cnf,andL)
import Minisat

data Color = Red | Blue | Green deriving (Enum,Show,Read,Typeable)
instance Finite Color where

d1 = dim 4
d2 = dimS ["a","cde","tom"]
d3 = dimL [True,False]
d4 = dimE "Color" Blue
d5 = expand [2,3]


s1 = partial (expand [2,2]) (\ [x,y]-> if even x then (Just True) else Nothing)
s2 = universe (expand [2,2]) (\ [x,y]-> x+y)
s3 = fromIndexList 'z' (expand [3,2]) [[0,1],[2,0],[1,1]]
s4 = fromFiniteList True [dimE "Bool" True,dimE "Color" Red] [(True,Blue),(False,Red)]

units = many [dim 5] [[i] | i <- [0..4]]
pairs = manyD [dim 5,dimE "Bool" True] [(3::Int,True),(4,False),(0,True),(1,False)]
(n',square) = enum f [dim 2,dim 3] 1 where f xs n = Just(n+1,LetterP n)


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

(_,q) = enum f [pd,pd] 1
  where f xs n = if elem xs indexes
                    then Just(n+1,LetterP n) else Nothing
        
        indexes = (map (kIndex [pd,pd] . toIndex [pd,pd]) tuples)

cross = join 0 q q    


s5 = unary (\ xs p -> conj (LetterP 99) p) q

counter xs n = Just(n+1,LetterP n)                                
(_,six) = enum counter [dim 3, dim 2] 1

p1 = one six
p2 = none six
p3 = some six
p4 = full six

(ii,y1) = enum counter [dim 4] 1
(_,y2) = enum counter [dim 4] ii


p5 = subset y1 y2
p6 = funDep [0][1] p
p7 = funDep [1][0] p

-------------------------------------------

numC = 4
graph::[(Int,Int)]
graph = [(1,2),(2,3),(3,4),(4,5),
         (5,1),(1,6),(2,7),(3,8),
         (4,9),(5,0),(6,8),(7,9),
         (8,0),(9,6),(0,7)]
         
colors = take numC 
           ["Red","Blue","Green",
            "Yellow","Purple","Orange",
            "Violet","Indigo"]
         
edges  = manyD [dim 10,dim 10] graph    
color = manyD [dimS colors] colors

(_,coloring) = enum f [dim 10,dimS colors] 1 
     where f [i,j] n = Just(n+1,LetterP n)
     
same = project [2,0,1] (                     -- (x,y,c1)
       select (\ [y,c2,x,c1] -> c1==c2) (    -- (y,c1,x,c1)
       join 1                                -- (y,c2,x,c1)
              coloring                       -- (y,c2)
              (project [2,0,1]               -- (y,x,c1)
                       (join 1               -- (x,c1,y)
                             coloring        -- (x,c1)
                             edges))))       -- (x,y)   
                             
colorProp = [ none same
            , full (project [0] coloring)]
                 
colorCnf = cnf (andL colorProp)
colorFile = putClauses "color.cnf" colorCnf

----------------------------------------------------

qsize = 4
(_,queens) = enum f (expand [qsize,qsize]) 1 
     where f [i,j] n = Just(n+1,LetterP n)
     
q2 = (select (\ [i,j]->i==0) queens)     
p8 = one (select (\ [i,j]->i==0) queens)

fdCol r = funDep [0] [1] r

fdRow r = funDep [1] [0] r

diag a b [i,j] n = (a==i+n && b==j+n)||(a==i-n && b==j-n)||
                   (a==i+n && b==j-n)||(a==i-n && b==j+n)
                   
qds = select (\ xs -> any (diag 2 2 xs) [1.. qsize] ) queens    

diagonal a b (r@(FA [n,m] tab)) = select alongDiag r
  where alongDiag xs = any (diag a b xs) [1.. (max (dimSize m) (dimSize n))-1] 
  
noneOnDiag r = full(unary f r)
  where f [a,b] p = imply p (none(diagonal a b r))  
  
p9 =  noneOnDiag queens  

eachRow r = full(project [0] r)
                   
queenProp = [ eachRow queens,
              fdCol queens,fdRow queens,noneOnDiag queens]  
              
queenCNF = cnf(andL queenProp)
queenFile = putClauses "queen.cnf" queenCNF              