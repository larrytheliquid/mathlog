module Hw1 where
import NatDed
import Blocks
import SimpleProp

p' = LetterP "p"
q' = LetterP "q"
r' = LetterP "r"
s' = LetterP "s"
t' = LetterP "t"

prob1 = Seq [p' /\ q'] $
  AndI (AndE2 (p (p' /\ q'))) (AndE1 (p (p' /\ q')))

prob2 = Seq [(p' /\ q') /\ r' , s' /\ t'] $
  AndI
    (AndE2 (AndE1 (p ((p' /\ q') /\ r'))))
    (AndE1 (p (s' /\ t')))


