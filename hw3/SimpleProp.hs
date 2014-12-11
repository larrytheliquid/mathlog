module SimpleProp where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)

data Prop a = 
     LetterP a
   | AndP (Prop a) (Prop a)
   | OrP (Prop a) (Prop a)
   | ImpliesP (Prop a) (Prop a)
   | NotP (Prop a)
 deriving Eq

--- Some syntactic sugar.
infixr 3 /\
infixr 2 \/
infixr 1 ~>

(/\),(\/),(~>) :: Prop a -> Prop a -> Prop a
p /\ q = AndP p q
p \/ q = OrP p q
p ~> q = ImpliesP p q

-- A valuation function

value :: (t -> Bool) -> Prop t -> Bool
value vf (NotP x) = not (value vf x)
value vf (AndP x y) = (value vf x) && (value vf y)
value vf (OrP x y) = (value vf x) || (value vf y)
value vf (ImpliesP x y) = if (value vf x) then (value vf y) else True
value vf (LetterP x) = vf x

------------------------------------------------------------------------
-- pretty printing Prop

class PPLetter a where
  ppLetter :: a -> Doc

instance PPLetter Int where
  ppLetter a = text ("p"++show a)

instance PPLetter Integer where
  ppLetter a = text ("p"++show a)

instance PPLetter Char where
  ppLetter = PP.char

instance PPLetter a => PPLetter [a] where
  ppLetter = PP.hcat . (map ppLetter)

data Name = A | B | C deriving (Eq,Ord,Show)  
instance PPLetter Name where
  ppLetter A = text "A"
  ppLetter B = text "B"
  ppLetter C = text "C"
  
  
class PP a where
  pp :: a -> Doc

instance PP Bool where
  pp True = text "True"
  pp False = text "False"

precedence (NotP _) = 5
precedence (AndP _ _) = 4
precedence (OrP _ _) = 3
precedence (ImpliesP _ _) = 2
precedence _ = 1

parens n (term@(LetterP _)) = pp term
parens n (term@(NotP _)) = pp term
parens n term | n /= precedence term = PP.parens (pp term)
              | otherwise = pp term

ands (AndP x y) = ands x ++ ands y
ands x = [x]

ors (OrP x y) = ors x ++ ors y
ors x = [x]

instance PPLetter a => PP(Prop a) where   
  pp (LetterP a) = ppLetter a
  pp (NotP t) = text "~" <> parens 1 t
  pp (p@(AndP x y)) = -- PP.sep [ parens 4 x, text "/\\", parens 4 y]
    PP.fsep (PP.punctuate (text " /\\") (map (parens 4) (ands p)))
  pp (p@(OrP x y)) = PP.sep [ parens 3 x, text "\\/", parens 3 y]
    -- PP.fsep (PP.punctuate (text " \\/") (map (parens 3) (ors p)))
  pp (ImpliesP x y) = PP.sep [ parens 3 x, text "=>", parens 2 y]

instance PPLetter a => Show (Prop a) where
  show x = render (pp x)
  
-------------------------------------
-- This is class hackery, use at your own risk

instance Num a => Num (Prop a) where
  fromInteger n = LetterP (fromInteger n)
  x + y = OrP x y
  x * y = AndP x y
  abs x = undefined
  signum x = undefined
  negate = undefined



 