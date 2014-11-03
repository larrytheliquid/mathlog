module Tactics where
import Text.PrettyPrint.HughesPJ
import Print
import Control.Monad

data Tag a = Focus a | QED | Open

instance (Show a) => Show (Tag a) where
  showsPrec n QED = ("QED"++)
  showsPrec n Open = ("Open"++) 
  showsPrec n (Focus a) = ("Focus: "++) . shows a

instance (PP a) => PP (Tag a) where
  pp QED = text "QED" 
  pp Open = text "Open"
  pp (Focus a) = text "Focus: " $$ nest 8 (pp a)

prove :: (PP s) => s -> [s -> Maybe [s]] -> Doc
prove s rs = docify [ head $ replaySteps [] [s] rs ]

-- replay :: (PP s) => s -> [s -> Maybe [s]] -> Doc
replay s rs = docify $ reverse $ replaySteps [] [s] rs 

replaySteps script [] [] = (QED, Just []):script
replaySteps script ss [] = (Open, Just ss):script
replaySteps script (s:ss) (r:rs) = 
     case r s of
         Just ss' -> replaySteps ((Focus s,Just ss'):script) (ss'++ss) rs
         Nothing -> (Focus s,Nothing):script

docify :: (PP s) => [(Tag s, Maybe [s])] -> Doc
docify [] = empty
docify ((s@(Focus _),Just ss):ps) = (pp s $$ nest 3 (ppSs ss)) $$ docify ps
docify ((s@(Focus _),Nothing):ps) = ((pp s) <+> (text "Fails")) $$ docify ps
docify ((Open,Just ss):ps) = (text "Remaining Subgoals:" $$ nest 3 (ppSs ss)) $$ docify ps
docify ((QED,Just ss):ps) = ((text "QED") <+> nest 3 (ppSs ss)) $$ docify ps

ppSs x = vcat $ punctuate (text ";") (map pp x)

-------------------------------------------------------------
-- Some basic tacticals, patterned a little after LCF
--  These should be agnostic to the definition of a sequent
--

-- orElse :: (a -> Maybe [a]) -> (a -> Maybe [a]) -> a -> Maybe [a]
orElse r1 r2 s = r1 s `mplus` r2 s

idTac s = return [s]

failTac s = mzero

oneOf :: MonadPlus m => [s -> m [s]] -> s -> m [s]
oneOf = foldl orElse failTac

try r = orElse r idTac

--
-- thenL takes a list of rules, one for each generated subgoal
--
-- thenL :: (a -> Maybe [a]) -> [(a -> Maybe [a])] -> a -> Maybe [a]
thenL r1 r2s s = do 
  ss <- r1 s
  zipTac r2s ss

--
-- thenOne takes one second rule, which is only applied to the first generated subgoal
-- thenAll is similar, but applies to all generated subgoals
--

thenOne r1 r2 = thenL r1 (r2:repeat idTac)

thenAll r1 r2 = thenL r1 (repeat r2)

-- repeat is your basic iterator
-- this version of repeat does not have a progress check; it is not safe for unfailing tactics

repeatT r = orElse (thenAll r (repeatT r)) idTac


zipTac rs ss = zipTacA rs ss []
  where
     zipTacA _ [] acc = return acc
     zipTacA (r:rs) (s:ss) acc = do 
       ss' <- r s
       zipTacA rs ss (ss'++acc)

