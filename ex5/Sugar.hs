module Sugar where
import Prelude hiding (all)
import Term
import Formula
import Print

infixr 7 /\
infixr 8 \/
infixr 6 ~>

(/\), (\/), (~>) :: Formula r f v -> Formula r f v -> Formula r f v
a /\ b = Conn And [a,b]
a \/ b = Conn Or [a,b]
a ~> b = Conn Imp [a,b]
nt a = Conn Not [a]

all v f = Quant All v f
ex v f = Quant Exist v f

prop :: String -> Formula String String String
prop a = Rel a []

(===) :: NumSkel -> NumSkel -> Formula String String String
a === b = Rel "=" [term a,term b]

data NumSkel = Plus NumSkel NumSkel
             | Minus NumSkel NumSkel
             | Negate NumSkel
             | Mult NumSkel NumSkel
             | Abs NumSkel
             | Signum NumSkel
             | FromInteger Integer
             | TVar String
             | Term (Term String String)
                  deriving (Eq, Show)

instance Num (NumSkel) where
    (+) = Plus
    (-) = Minus
    negate = Negate
    (*) = Mult
    abs = Abs
    signum = Signum
    fromInteger = FromInteger

term :: NumSkel -> Term String String
term (Term t) = t
term (TVar t ) = Var t
term (Plus a b) = Fun False "+" [term a,term b]
term (Minus a b) = Fun False "-" [term a,term b]
term (Negate a) = Fun False "negate" [term a]
term (Mult a b) = Fun False "*" [term a,term b]
term (FromInteger n) 
     | n >= 0 = peano n
     | n < 0 = Fun False "negate" [peano (-n)]

peano 0 = Fun False "0" []
peano (n+1) = Fun False "S" [peano n]

s f = Fun False "S" [f]