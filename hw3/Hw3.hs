module Main  where

import SimpleProp

----------------------------------------------------------------------

data SProp a = T (Prop a) | F (Prop a) deriving Show

data Discrim a = All (Discrim' a) | Ts (Discrim' a)

data Discrim' a = Alpha a a | Beta a a | Lit a

----------------------------------------------------------------------

restrictD :: Discrim a -> Discrim a
restrictD (All x) = Ts x
restrictD x = x

----------------------------------------------------------------------

discrim :: SProp a -> Discrim (SProp a)
discrim x = case x of
  T TruthP         -> All $ Lit (T TruthP)
  T AbsurdP        -> All $ Lit (T AbsurdP)
  T (LetterP s)    -> All $ Lit (T (LetterP s))
  T (AndP x y)     -> All $ Alpha (T x) (T y)
  T (OrP x y)      -> All $ Beta (T x) (T y)
  T (ImpliesP x y) -> All $ Beta (F x) (T y)
  T (NotP x)       -> discrim (F x)

  F TruthP         -> All $ Lit (T AbsurdP)
  F AbsurdP        -> All $ Lit (T TruthP)
  F (LetterP s)    -> All $ Lit (F (LetterP s))
  F (AndP x y)     -> All $ Beta (F x) (F y)
  F (OrP x y)      -> All $ Alpha (F x) (F y)
  F (ImpliesP x y) -> Ts  $ Alpha (T x) (F y)
  F (NotP x)       -> restrictD $ discrim (T x)

----------------------------------------------------------------------

restrict :: [SProp a] -> [SProp a]
restrict [] = []
restrict (T p : ps) = T p : restrict ps
restrict (F _ : ps) = restrict ps

----------------------------------------------------------------------

process :: [SProp a] -> [[SProp a]]
process [] = [[]]
process (p : ps) =
  case discrim p of
    All x -> f x ps
    Ts x  -> f x (restrict ps)
  where
  f (Lit x) qs = map (x:) (process qs)
  f (Alpha x y) qs = process (x : y : qs)
  f (Beta x y) qs = process (x : qs) ++ process (y : qs)

tabulate :: Prop a -> [[SProp a]]
tabulate p = process [F p]

----------------------------------------------------------------------

toEither :: [SProp a] -> [Either a a]
toEither [] = []
toEither (T (LetterP a) : ps) = Left a : toEither ps
toEither (F (LetterP a) : ps) = Right a : toEither ps
toEither (_ : ps) = toEither ps

both :: Eq a => [Either a a] -> Maybe [Either a a]
both = flip foldl (Just []) $
  \ m x -> case m of
    Nothing -> Nothing
    Just ps -> case x of
      Left p -> if any (== Right p) ps then Nothing else Just (Left p:ps)
      Right p -> if any (== Left p) ps then Nothing else Just (Right p:ps)

contra :: Eq a => [SProp a] -> Bool
contra = maybe True (const False) . both . toEither

contras :: Eq a => [[SProp a]] -> Bool
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