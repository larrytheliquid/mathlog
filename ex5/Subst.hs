module Subst(Subst, emptySubst, (|=>), (|->), (|/->)) where

type Subst v m = v -> m v

emptySubst :: Monad m => Subst v m
emptySubst v = return v

-- Substituting the variable v with the term t

(|->) :: (Eq v, Monad m) => v -> m v -> Subst v m
(v |-> t) v' | v == v'   = t
             | otherwise = emptySubst v'

-- Composing two substitutions
(|=>) :: Monad m => Subst v m -> Subst v m -> Subst v m
s1 |=> s2 = (s1 =<<) . s2

-- Removing a variable from a substitution
(|/->) :: (Eq v, Monad m) => v -> Subst v m -> Subst v m
(v |/-> s) v' | v == v'   = return v'
              | otherwise = s v'
