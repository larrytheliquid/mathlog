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

f3 = toFormula "(exists x. P(x)) --> (exists y. P(y))"
s3 = SequentM [] [f3]

m3 :: Proof Doc
m3 = replayM s3 [impliesR 1,exL 1,exR (toTerm "?b") 1, axiom 1 1]
ex3 = evalStateT m3 ([1..],[s3])

----------------------------------------------------------------------

f4 = toFormula "(exists y. Q(y) & P(y)) --> ((exists x. P(x)) & (exists x. Q(x)))"
s4 = SequentM [] [f4]

m4 :: Proof Doc
m4 = replayM s4 [impliesR 1,exL 1,andL 1,andR 1,exR (toTerm "?b") 1, axiom 2 1,exR (toTerm "?b") 1, axiom 1 1]
ex4 = evalStateT m4 ([1..],[s4])

----------------------------------------------------------------------

f2' = toFormula "(forall x. P(x)) --> (exists y. P(y))"
s2' = SequentM [] [f2']

m2' :: Proof Doc
m2' = (replayM s2' [iR,alL (toTerm "?b"),eR (toTerm "?b"), ax])
ex2' = evalStateT m2' ([1..],[s2'])

----------------------------------------------------------------------

f3' = toFormula "(exists x. P(x)) --> (exists y. P(y))"
s3' = SequentM [] [f3']

m3' :: Proof Doc
m3' = replayM s3' [iR,eL,eR (toTerm "?b"), ax]
ex3' = evalStateT m3' ([1..],[s3'])

----------------------------------------------------------------------

f4' = toFormula "(exists y. Q(y) & P(y)) --> ((exists x. P(x)) & (exists x. Q(x)))"
s4' = SequentM [] [f4']

m4' :: Proof Doc
m4' = replayM s4' [iR,eL,aL,aR,eR (toTerm "?b"), ax,eR (toTerm "?b"), ax]
ex4' = evalStateT m4' ([1..],[s4'])

----------------------------------------------------------------------