module Term (Term (..), variables, newVar, newFun, subTerm) where
import Data.Char

-- Note that constants are represented by functions with no arguments
-- Functions can be skolem functions and are then marked as such

data Term f v = Var v 
              | Fun Bool f [Term f v] deriving Eq

-- Bind is substitution on Terms
instance Monad (Term f) where
  return v = Var v
  
  (Var v)      >>= s = s v
  (Fun b n ts) >>= s = Fun b n (map (>>= s) ts)

subTerm:: (v -> Term f v) -> Term f v -> Term f v
subTerm s t = t >>= s

variables :: Term f v -> [Term f v]
variables (Var v)  = [Var v]
variables (Fun s n ts) = concat (map variables ts)

newVar :: Int -> Term f String
newVar n = Var ("?" ++ intToString n)

newFun :: Int -> [Term String v] -> Term String v
newFun n ts = Fun True ("_" ++ intToString n) ts

intToString :: Int -> String
intToString n = if n < 26 then letter else intToString (n `div` 26) ++ letter
  where letter = [Data.Char.chr ((n `mod` 26) + 97)]