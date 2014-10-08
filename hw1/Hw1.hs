module Hw1 where
import NatDed
import Blocks
import SimpleProp

p' = LetterP "p"
q' = LetterP "q"
r' = LetterP "r"
s' = LetterP "s"
t' = LetterP "t"

-- p /\ q |- q /\ p
prob1 = Seq [p' /\ q'] $
  AndI (AndE2 (p (p' /\ q'))) (AndE1 (p (p' /\ q')))

-- (p /\ q /\ r), (s /\ t) |- q /\ s
prob2 = Seq [(p' /\ q') /\ r' , s' /\ t'] $
  AndI
    (AndE2 (AndE1 (p ((p' /\ q') /\ r'))))
    (AndE1 (p (s' /\ t')))

-- p -> (q -> r), p -> q |- p -> r
prob3 = Seq [p' ~> (q' ~> r') , p' ~> q'] $
  ImplyI p' $
  ImplyE
    (ImplyE (p p') (p (p' ~> q')))
    (ImplyE (p p') (p (p' ~> (q' ~> r'))))

-- p1 -> p2, ~p2 |- ~p1
prob4 = Seq [1~>2 , NotP 2] $
  NegI 1 $
    AbsurdI (ImplyE (p 1) (p (1~>2))) (p (NotP 2))

-- p1 \/ ~p1
prob5 = Neg2E $ NegI (NotP (1+NotP 1))
  (AbsurdI
    (OrI2 1 (NegI 1
      (AbsurdI
        (OrI1 (p 1) (NotP 1))
        (p ((NotP (1+NotP 1))))
      )
    ))
    (p ((NotP (1+NotP 1))))
   )
