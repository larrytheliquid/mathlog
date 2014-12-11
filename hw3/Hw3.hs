module Main  where

import SimpleProp
import Data.List

----------------------------------------------------------------------

data Sign = T | F deriving (Eq, Show)

type SProp a = (Sign , Prop a)
type SVar a = (Sign , a)

restrict :: [(Sign , a)] -> [(Sign , a)]
restrict [] = []
restrict ((T , p) : ps) = (T , p) : restrict ps
restrict ((F , _) : ps) = restrict ps

----------------------------------------------------------------------

process :: [SProp a] -> [SVar a] -> [[SVar a]]
process [] as = [as]
process (p : ps) as = case p of
  (s , LetterP a)     -> process ps ((s , a):as)
  (T , AndP x y)      -> process ((T , x):(T , y):ps) as
  (F , AndP x y)      -> process ((F , x):ps) as ++ process ((F , y):ps) as
  (T , OrP x y)       -> process ((T , x):ps) as ++ process ((T , y):ps) as
  (F , OrP x y)       -> process ((F , x):(F , y):ps) as
  (T , ImpliesP x y)  -> process ((F , x):ps) as ++ process ((T , y):ps) as
  (F , ImpliesP x y)  -> process ((T , x):(F , y):restrict ps) (restrict as)
  (T , NotP x)        -> process ((F , x):ps) as
  (F , NotP x)        -> process ((T , x):restrict ps) (restrict as)

----------------------------------------------------------------------

tabulate :: Prop a -> [[SVar a]]
tabulate p = process [(F , p)] []

----------------------------------------------------------------------

both :: Eq a => [SVar a] -> Maybe [SVar a]
both = flip foldl (Just []) $
  \ m x -> case m of
    Nothing -> Nothing
    Just ps -> case x of
      (T , p) -> if any (== (F , p)) ps then Nothing else Just ((T , p):ps)
      (F , p) -> if any (== (T , p)) ps then Nothing else Just ((F , p):ps)

contra :: Eq a => [SVar a] -> Bool
contra = maybe True (const False) . both

contras :: Eq a => [[SVar a]] -> Bool
contras = all contra

----------------------------------------------------------------------

tableau :: Eq a => Prop a -> Bool
tableau = contras . tabulate

----------------------------------------------------------------------

eg1 = 0 ~> 0

eg2 = 0 ~> (0 /\ 0)

eg3 = 0 \/ (NotP 0)

eg4 = 0 ~> ((0 ~> 1) ~> 1)

eg5 = NotP 0 ~> NotP 0

----------------------------------------------------------------------