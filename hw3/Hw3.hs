module Main  where

import SimpleProp
import Data.List

----------------------------------------------------------------------

data Sign = T | F deriving (Eq, Ord, Show)

type SProp a = (Sign , Prop a)
type SVar a = (Sign , a)

restrict :: [(Sign , a)] -> [(Sign , a)]
restrict [] = []
restrict ((T , p) : ps) = (T , p) : restrict ps
restrict ((F , _) : ps) = restrict ps

----------------------------------------------------------------------

process :: Eq a => [SProp a] -> [SVar a] -> [[[SVar a]]]
process [] as = [[as]]
process xs as = [ process1 x (delete x xs) as | x <- xs ]

processN :: Eq a => [SProp a] -> [SVar a] -> [[SVar a]]
processN ps as = nub . concat $ process ps as

process1 :: Eq a => SProp a -> [SProp a] -> [SVar a] -> [[SVar a]]
process1 (s , LetterP a)     ps as = processN ps ((s , a):as)
process1 (T , AndP x y)      ps as = processN ((T , x):(T , y):ps) as
process1 (F , AndP x y)      ps as = processN ((F , x):ps) as ++ processN ((F , y):ps) as
process1 (T , OrP x y)       ps as = processN ((T , x):ps) as ++ processN ((T , y):ps) as
process1 (F , OrP x y)       ps as = processN ((F , x):(F , y):ps) as
process1 (T , ImpliesP x y)  ps as = processN ((F , x):ps) as ++ processN ((T , y):ps) as
process1 (F , ImpliesP x y)  ps as = processN ((T , x):(F , y):restrict ps) (restrict as)
process1 (T , NotP x)        ps as = processN ((F , x):ps) as
process1 (F , NotP x)        ps as = processN ((T , x):restrict ps) (restrict as)

----------------------------------------------------------------------

tabulate :: Eq a => Prop a -> [[[SVar a]]]
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

closed :: Eq a => [[[SVar a]]] -> Bool
closed = any (all contra)

----------------------------------------------------------------------

tableau :: Eq a => Prop a -> Bool
tableau = closed . tabulate

----------------------------------------------------------------------

eg1 = 0 ~> 0

eg2 = 0 ~> (0 /\ 0)

eg3 = 0 \/ (NotP 0)

eg4 = 0 ~> ((0 ~> 1) ~> 1)

eg5 = NotP 0 ~> NotP 0

----------------------------------------------------------------------