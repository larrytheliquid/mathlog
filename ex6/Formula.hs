module Formula where

import Term
import Subst
import Control.Monad
import Control.Monad (guard)
import Data.List (nub)

data Formula r f v = Rel r [Term f v]
                   | Conn Cs [Formula r f v]
                   | Quant Qs v (Formula r f v) 
                   deriving Eq

data Qs = All | Exist deriving Eq

data Cs = And | Or | Imp 
        | T | F | Not 
        deriving Eq

subst :: Eq v => (v -> Term f v) -> Formula r f v -> Formula r f v
subst s (Rel r ts)    = Rel r (map (s =<<) ts)
subst s (Conn c fs)   = Conn c (map (subst s) fs)
subst s (Quant q v f) = Quant q v (subst (v |/-> s) f)


vars :: (Eq f, Eq v) => Formula r f v -> [Term f v]
vars (Rel r ts)    = nub $ concat $ map variables ts
vars (Conn c fs)   = nub $ concat $ map vars fs
vars (Quant q v f) = nub $ filter (/= (Var v)) $ vars f
