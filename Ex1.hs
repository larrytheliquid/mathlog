{-# LANGUAGE StandaloneDeriving, DeriveFoldable #-}
module Ex1 where
import SimpleProp
import Data.List
import Control.Monad
import qualified Data.Foldable as F
deriving instance F.Foldable Prop

----------------------------------------------------------------------

imp :: Bool -> Bool -> Bool
imp x y = if x then y else True

equiv :: Bool -> Bool -> Bool
equiv x y = imp x y && imp y x

----------------------------------------------------------------------

addvars :: Ord a => Prop a -> [a] -> [a] 
addvars p xs = sort (nub (F.toList p ++ xs))

vars :: Ord a => Prop a -> [a]
vars = flip addvars []

----------------------------------------------------------------------

valuations :: Int -> [[Bool]]
valuations n = replicateM n [True,False]

assigns :: [Int] -> [[(Int,Bool)]]
assigns xs = map (zip xs) (valuations (length xs))

----------------------------------------------------------------------

satis :: Prop Int -> [(Int,Bool)] -> Bool
satis (LetterP a) v = maybe (error "var not in valuation") id (lookup a v)
satis (AndP x y) v = satis x v && satis y v
satis (OrP x y) v = satis x v || satis y v
satis (ImpliesP x y) v = satis x v `imp` satis y v
satis (NotP x) v = not (satis x v)
satis AbsurdP v = False
satis TruthP v = True

----------------------------------------------------------------------

taut:: Prop Int -> Bool
taut p = all (satis p) (assigns (vars p))

equal :: Prop Int -> Prop Int -> Bool
equal p q = all
  (\vs -> satis p vs `equiv` satis q vs)
  (assigns (union (vars p) (vars q)))

----------------------------------------------------------------------