module Print where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),($$),render)

import Term
import Formula

class PP a where
  pp :: a -> Doc

instance PP Char where
  pp = PP.char

instance PP a => PP [a] where
  pp = PP.hcat . (map pp)


instance (PP f, PP v) => PP (Term f v) where
  pp (Var v) = pp v
  pp (Fun True n ts) = pp n <> (PP.parens $ PP.sep $ PP.punctuate PP.comma (map pp ts))
  pp (Fun False n []) = pp n 
  pp (Fun False n ts) = pp n <> (PP.parens $ PP.sep $ PP.punctuate PP.comma (map pp ts))

instance (PP f, PP v) => Show (Term f v) where
  show = render . pp

instance PP Qs where
  pp All = text "ALL"
  pp Exist = text "EX"

instance PP Cs where
  pp And   = text "&"
  pp Or    = text "|"
  pp Imp   = text "-->"
  pp Not   = text "~"
  pp T     = text "TRUE"
  pp F     = text "FALSE"

instance (PP r, PP f, PP v) => PP (Formula r f v) where
  pp (Rel v [])     = pp v 
  pp (Rel v ts)     = pp v <> (PP.parens $ PP.sep $ PP.punctuate PP.comma (map pp ts))
  pp (Conn c [])    = pp c
  pp (Conn c [x])   = pp c <> pp x
  pp (Conn c [x,y]) = PP.parens (pp x <+> pp c <+> pp y)
  pp (Quant q v f)  = PP.parens (pp q <+> pp v <> period <+> pp f)

instance (PP r, PP f, PP v) => Show (Formula r f v) where
   show = render . pp

period = PP.char '.'
