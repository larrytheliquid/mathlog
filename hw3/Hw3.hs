module Main  where

import SimpleProp

----------------------------------------------------------------------

data SProp a = T (Prop a) | F (Prop a)

data Discrim a = All (Discrim' a) | Ts (Discrim' a)

data Discrim' a = Alpha a a | Beta a a | Lit a

----------------------------------------------------------------------

restrict :: Discrim a -> Discrim a
restrict (All x) = Ts x
restrict x = x

----------------------------------------------------------------------

discrim :: SProp a -> Discrim (SProp a)
discrim x = case x of
  T TruthP         -> All $ Lit (T TruthP) -- TODO
  T AbsurdP        -> All $ Lit (T AbsurdP) -- TODO
  T (LetterP s)    -> All $ Lit (T (LetterP s))
  T (AndP x y)     -> All $ Alpha (T x) (T y)
  T (OrP x y)      -> All $ Beta (T x) (T y)
  T (ImpliesP x y) -> All $ Beta (F x) (T y)
  T (NotP x)       -> discrim (F x)

  F TruthP         -> All $ Lit (F TruthP) -- TODO
  F AbsurdP        -> All $ Lit (F AbsurdP) -- TODO
  F (LetterP s)    -> All $ Lit (F (LetterP s))
  F (AndP x y)     -> All $ Beta (F x) (F y)
  F (OrP x y)      -> All $ Alpha (F x) (F y)
  F (ImpliesP x y) -> Ts  $ Alpha (T x) (F y)
  F (NotP x)       -> restrict $ discrim (T x)

----------------------------------------------------------------------

-- process :: [Prop a] -> [[Prop a]]
-- process [] = [[]]
-- process (p : ps) =
--   case (discrim p) of
--     Lit x -> map (x:) (process ps)
--     Alpha x y -> process (x : y : ps)
--     Beta x y -> process (x : ps) ++ process (y : ps)

-- tabulate :: Prop a -> [[Prop a]]
-- tabulate p = process [NotP p]

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

-- tableau :: Eq a => Prop a -> Bool
-- tableau = contras . tabulate

-- tableau2 :: Eq a => Prop a -> Bool
-- tableau2 = contras . tabulate2

-- ----------------------------------------------------------------------

-- eg0 =  0 /\ 1

-- eg1 =  0 \/ (1 /\ 2)

-- eg2 = 0 \/ (1 /\ 2) ~> ((0 \/ 1) /\ (0 \/ 2))

-- eg3 = (0 ~> (1 ~> 2)) ~> ((0 ~> 1) ~> (0 ~> 2))

-- ----------------------------------------------------------------------