module Examples where
import Prelude hiding (all)
import Term
import Formula
import Print
import Parser
import LK

fml = toFormula "p&q --> q&p"

m3 :: Formula String String String
m3 = Conn Imp [Quant All "a" (Rel "O" [Var "a",Var "a"]),
               Quant All "a" $
                  Quant Exist "b" $
                     Rel "O" [Var "a", Var "b"]]

m3' = toFormula "(forall a. O(a,a)) --> (forall a. exists b. O(a,b))" 

fitting138 :: Formula String String String
fitting138 = toFormula "(forall x. P(x) | Q(x)) --> (exists x. P(x)) | (forall x. Q(x))"

emptyM3Proof = proveM (SequentM [] [m3]) []
m3Proof = proveM (SequentM [] [m3]) [iR,alR,alL (toTerm "?b"),eR (toTerm "?b"),ax]