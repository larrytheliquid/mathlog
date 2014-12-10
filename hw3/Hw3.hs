module Main  where

import SimpleProp

----------------------------------------------------------------------

type SProp a = (Bool, Prop a)

----------------------------------------------------------------------

data Discrim a = Alpha a a | Beta a a | Lit a

discrim :: SProp a -> Discrim (Prop a)

discrim (True , TruthP) = Lit TruthP
discrim (True , AbsurdP) = Lit AbsurdP
discrim (True , LetterP s) = Lit (LetterP s)
discrim (True , AndP x y) = Alpha x y
discrim (True , OrP x y) = Beta x y
discrim (True , ImpliesP x y) = Beta (NotP x) y
discrim (True , NotP x) = undefined

discrim (False , OrP x y) = Alpha (NotP x)  (NotP y)
discrim (False , ImpliesP x y) = Alpha x (NotP y)
discrim (False , AndP x y) = Beta (NotP x) (NotP y)
discrim (False , NotP x) = undefined -- discrim x
discrim (False , TruthP) = Lit AbsurdP
discrim (False , AbsurdP) = Lit TruthP
discrim (False , LetterP s) = Lit (NotP (LetterP s))

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
toEither ((True , LetterP a) : ps) = Left a : toEither ps
toEither ((False , LetterP a) : ps) = Right a : toEither ps
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