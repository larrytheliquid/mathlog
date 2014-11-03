module Tableau where

import Formula
import Subst
import Term
import Control.Monad.State
import Data.List((\\))
import Blocks
import Print
import Parser

-- Discrimination now has two more possibilities

data Discrim v a = Alpha a a | Beta a a | Lit a 
               | Gamma v a | Delta v a
 deriving Show

notP (Conn Not [x]) = x
notP x = Conn Not [x]

-- discrim is like before, but now over (Formula p f v) rather then (Prop a)

discrim :: Formula p f v -> Discrim v (Formula p f v)
discrim (p@(Rel r ts)) = Lit p
discrim (Conn T []) = Lit (Conn T [])
discrim (Conn F []) = Lit (Conn F [])
discrim (Conn And [x,y]) = Alpha x y
discrim (Conn Or [x,y]) = Beta x y
discrim (Conn Imp [x,y]) = Beta (notP x) y
discrim (Conn Not [x]) =
  case x of
    (Rel r ts) -> Lit(notP x)
    (Conn T []) -> Lit(Conn F [])
    (Conn F []) -> Lit(Conn T []) 
    (Conn And [x,y]) -> Beta (notP x) (notP y)
    (Conn Or [x,y]) -> Alpha (notP x) (notP y)
    (Conn Imp [x,y]) -> Alpha x (notP y) 
    (Conn Not [x]) -> discrim x
    (Quant All v f) -> Delta v (notP f)
    (Quant Exist v f) -> Gamma v (notP f)
discrim (Quant All v f) = Gamma v f
discrim (Quant Exist v f) = Delta v f


-----------------------------------------------------
-- unification over terms

occurs :: Eq v => v -> Term f v -> Maybe ()
occurs v t = hasV (variables t)
  where hasV [] = Just ()
        hasV (Var u : xs) | v==u = Nothing
        hasV (x:xs) = hasV xs


unify :: (Eq f, Eq v) => Term f v -> Term f v -> Maybe (Subst v (Term f))
unify (Var v) (Var u) 
  | u==v = return emptySubst
unify (Var v) y =
  do { occurs v y
     ; return(v |-> y)}
unify y (Var v) =
  do { occurs v y
     ; return (v |-> y) } 
unify (Fun _ f ts) (Fun _ g ss) 
  | f==g = unifyLists ts ss
unify x y = Nothing

unifyLists :: (Eq f, Eq v) => [Term f v] -> [Term f v] -> Maybe (Subst v (Term f))
unifyLists [] [] = Just emptySubst
unifyLists [] (x:xs) = Nothing
unifyLists (x:xs) [] = Nothing
unifyLists (x:xs) (y:ys) = 
  do { s1 <- unify x y
     ; s2 <- unifyLists 
               (map (subTerm s1) xs) 
               (map (subTerm s1) ys)
     ; return(s2 |=> s1)}
 

unifyForm (Rel x ts) (Rel y ss) 
  | x==y = unifyLists ts ss
unifyForm (Conn c1 ts) (Conn c2 ss) 
  | c1==c2 = unifyForms ts ss
unifyForm x y = Nothing

unifyForms [] [] = Just emptySubst
unifyForms [] (x:xs) = Nothing
unifyForms (x:xs) [] = Nothing
unifyForms (x:xs) (y:ys) = 
  do { s1 <- unifyForm x y
     ; s2 <- unifyForms (map (subst s1) xs) 
                        (map (subst s1) ys)
     ; return(s2 |=> s1)}
     
conjugates x y = 
  case unifyForm x (notP y) of
    Just s -> Just(s,subst s x,subst s y)
    Nothing -> case unifyForm (notP x) y of
                 Just s -> Just(s,subst s x,subst s y)
                 Nothing -> Nothing

someConj x [] = Nothing
someConj x (y:ys) = 
  case conjugates x y of
    Just triple -> Just triple
    Nothing -> someConj x ys
        
---------------------------------------------
-- type FormulaS = Formula String String String

data Tree 
  = Direct (FormulaS) Tree
  | Branch Tree Tree 
  | Leaf 
  | Closed FormulaS FormulaS
  

extendTree p Leaf = p
extendTree p (Direct q t) = 
   Direct q (extendTree p t)
extendTree p (Branch x y) = 
   Branch (extendTree p x) (extendTree p y)
extendTree p (Closed x y) = 
   Closed x y

-- make 1 and 2 elements trees 
single p = Direct (p) Leaf
double p q = Direct (p) (single q)
                    
extendTruths (True,p) truths = p:truths
extendTruths (False,p) truths = (notP p):truths

                
-- invariant: elements of the list are in 
-- the tree but not yet "used"                    

tabTree :: [FormulaS] -> Tree -> State Int Tree
tabTree [] tree = return tree
tabTree (x:xs) tree =
  case discrim x of
    Lit p -> tabTree xs tree
    Alpha a b -> tabTree (a:b:xs) (extendTree (double a b) tree)
    Beta a b -> do x <- tabTree (a:xs) (single a)
                   y <- tabTree (b:xs) (single b)
                   return(extendTree (Branch x y) tree)
    Gamma s f -> 
      do { t <- freshTerm
         ; let form = (subst (s |-> t) f)
         ; tabTree (form:xs) (extendTree (single form) tree) }
    Delta s f ->
      do { t <- freshSkolem s f
         ; tabTree (t:xs) (extendTree (single t) tree)}


freshInt :: State Int Int
freshInt = do { n <- get; put (n+1); return n}

freshTerm = do { n <- freshInt; return (Var ("n"++show n))}

freshSkolem :: String -> FormulaS -> State Int FormulaS
freshSkolem v form =
  do { n <- freshInt
     ; let us = vars form \\ [Var v]
           term = Fun True ("f"++show n) us
     ; return(subst (v |-> term) form)}  
  
try x = tblock tree
  where (tree,next) = runState (tabTree [notP x] (single (notP x))) 1



f0 = toFormula "(forall x. O(x,x)) --> (forall x. exists y. O(y,x))"
f1 = toFormula "(forall x. O(x,x)) --> (forall x. exists y. O(x,y))"
f2 = toFormula "(forall x. O(x,x)) --> (forall x. forall y. O(x,x) | O(y,y))"
f3 = toFormula "(forall x. O(x,x)) --> (forall x. forall y. O(x,x) & O(y,y))"

------------------------------------------------
-- code for printing and drawing trees

instance Show Tree where
  show Leaf = ""
  show (Branch x y) = "["++show x ++ "/\\"++ show y++"]"
  show (Direct (p) t) = "{"++show p++" "++show t++"}"
  show (Closed x y) = "X("++show x++","++show y++")"
  
lblock (p) = beside True (oneBlock "")(oneBlock (show p))
 
tblock Leaf = oneBlock ""
tblock (Direct x t) = above (center (lblock x) w) bottom
   where bottom = (tblock t)
         w = width bottom
tblock (Branch x y) = (sep True 2 bs)
    where bs = map (box 0 . tblock) [x,y]
          w = sum (map width bs)
tblock (Closed x y) = oneBlock(show(Closed x y))       


---------------------------------------------------
-- Now we will try a use the Tableau method without
-- actually constructing the Tree. Instead we will
-- construct a list of paths in the tree.
-- We only keep literals in the path.

isLit (p@(Rel r ts)) = True
isLit (Conn T []) = True
isLit (Conn F []) = True
isLit (Conn Not [t]) = isLit t
isLit x = False

cons3 x xs | not (isLit x) = xs
cons3 x xs | elem x xs = xs
cons3 x xs = x : xs
    
insert3 x xs = x :xs

tab3:: [FormulaS] -> [[FormulaS]] -> State Int [[FormulaS]]    
tab3 [] paths = return paths
tab3 (x:xs) paths =
  case discrim x of
    Lit p -> tab3 xs (map (cons3 p) paths)
    Alpha a b -> tab3 (insert3 a (insert3 b xs)) 
                      (map (cons3 a . cons3 b) paths)
    Beta a b ->
        do { ms <- tab3 (insert3 a xs) 
                        (map (cons3 a) paths)
           ; ns <- tab3 (insert3 b xs) 
                        (map (cons3 b) paths) 
           ; return (ms++ns)}
    Gamma v f ->                 
       do { t <- freshTerm
          ; let form = (subst (v |-> t) f)
          ; tab3 (form:xs) paths }
    Delta s f ->
      do { t <- freshSkolem s f
         ; tab3 (t:xs) paths }          


try2 x = runState (tab3 [notP x] [[]]) 1
       
----------------------------------------------------------------------

contra' :: (Eq v, Eq f, Eq a) =>
  [Formula a f v] -> [Formula a f v] -> Maybe (Subst v (Term f))
contra' (x:xs) ys = case someConj x ys of
  Nothing -> contra' xs ys
  Just (x,_,_) -> Just x
contra' [] ys = Nothing

contra xs = contra' xs xs

contras :: [[FormulaS]] -> Bool
contras (xs:xss) = case contra xs of
  Just s -> contras (map (map (subst s)) xss)
  Nothing -> False
contras [] = True

tableau = contras . fst . try2

{- tableau works on f0, f1, & f2 because
each yields terms that can be unified in each branch.

fst . try2 $ f0
[[~O(n3, f2()),O(n1, n1)]]
subst: n3 |-> n1, n1 |-> f2()

fst . try2 $ f1
[[~O(f2(), n3),O(n1, n1)]]
subst: n3 |-> n1, n1 |-> f2()

fst . try2 $ f2
[[~O(f2(), f2()),~O(f3(),f3()),O(n1, n1)]]
subst: n1 |-> f2()

fst . try2 $ f3
[[~O(f2(), f2()),O(n1, n1)],
 [~O(f3(), f3()),O(n1, n1)]]
subst1: n1 |-> f2()
FAIL

-}

----------------------------------------------------------------------                