module Ex5 where
import Prelude hiding (all)
import Term
import Formula
import Print
import Parser
import LK
import Control.Monad.State
import Text.PrettyPrint.HughesPJ(Doc)

----------------------------------------------------------------------

f2 = toFormula "(forall x. P(x)) --> (exists y. P(y))"
s2 = SequentM [] [f2]

m2 :: Proof Doc
m2 = (replayM s2 [impliesR 1,allL (toTerm "?b") 1,exR (toTerm "?b") 1, axiom 1 1])
ex2 = evalStateT m2 ([1..],[s2])

----------------------------------------------------------------------



----------------------------------------------------------------------