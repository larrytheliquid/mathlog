{-# Language FlexibleInstances #-}

module Blocks where

import SimpleProp
import Data.List ((\\))

-- These for pretty printing
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($+$),render)

-------------------------------------------------------
-- A Block is just a [String] with extra information

data Block = Block Int Int [String]

dim (Block w h xs) = (w,h)

--------------------------------------------------------

class Blockable t where
  toBlock :: t -> Block

instance Blockable String where
  toBlock = oneBlock

to x = toBlock(show x)

--------------------------------------------------------
-- useful helper functions

pad :: Int -> [String] -> [String]
pad n xs = xs ++ replicate n ""

fit :: Int -> String -> String
fit n xs | n==m = xs
         | n<m  = take n xs
         | n>m  = xs ++ replicate (n-m) ' '
  where m = length xs

maxx [] = 0
maxx xs = maximum xs

justifyH n xs = map (fit n) xs

justifyV n xs | n==height = ys
              | n < height = take n ys
              | n > height = replicate (n-height) blanks ++ ys -- bottom aligned
              | n > height = ys ++ replicate (n-height) blanks  -- top aligned
  where width = maxx(map length xs)
        height = length xs
        blanks = replicate width ' '
        ys = justifyH width xs

----------------------------------------------------
-- operations to create Blocks from primitive data

center (b@(Block w l s)) size | w >= size = b
center (b@(Block w l s)) size = lineUp[left,b,right]
  where delta = size - w
        leftsize = delta `div` 2
        rightsize = delta - leftsize
        left = oneBlock(fit leftsize "")
        right = oneBlock(fit rightsize "")

oneBlock :: String -> Block
oneBlock s = Block width (length zs) (map (fit width) zs)
   where width = maxx (map length zs)
         zs = lines s

block :: Int -> String -> Block
block n s = Block width (length zs) (map (fit (max width n)) zs)
   where width = maximum (map (\ s -> max n (length s)) zs)
         zs = lines s


manyBlock:: [String] -> Block
manyBlock xs = Block width height (justifyH width xs)
  where width = maxx(map length xs)
        height = length xs

docBlock:: Doc -> Block
docBlock doc = manyBlock xs
  where xs = lines(render doc)

------------------------------------------------------
-- Block to Block functions

padBlock:: Block -> Int -> Block
padBlock (Block w h xs) n
   | n==w = (Block w h xs)
   | n > w = Block n h (justifyH n xs)
   | n < w = Block n h (map (take n) xs)

indentBlock:: Int -> Block -> Block
indentBlock n (Block w h xs) = Block (n+w) h (map add xs)
  where add xs = replicate n ' ' ++ xs

beside:: Block -> Block -> Block
beside (Block w1 h1 xs) (Block w2 h2 ys) = Block (w1+w2) height zs
  where height = max h1 h2
        xs1 = justifyV height xs
        ys1 = justifyV height ys
        zs = zipWith (++) xs1 ys1

above:: Block -> Block -> Block
above (Block w1 h1 xs) (Block w2 h2 ys) = Block width (h1+h2) (xs1++ ys1)
  where width = max w1 w2
        xs1 = justifyH width xs
        ys1 = justifyH width ys

box:: Int -> Block -> Block
box margin (Block w h xs) = Block width height zs
  where width  = 1 + margin + w + margin + 1
        height = 1 + margin + h + margin + 1
        spaces = replicate margin ' '
        fill x = "|" ++ spaces ++ x ++ spaces ++ "|"
        edge = ["+" ++ replicate (w+2*margin) '-' ++ "+"]
        more = replicate margin (fill (replicate w ' '))
        zs = edge ++ more ++ (map fill xs) ++ more ++ edge
        

cap margin (Block w h xs) = Block width height zs
  where width  = 1 + margin + w + margin + 1
        height = 1 + margin + h + margin + 1
        spaces = replicate margin ' '
        fill x = "|" ++ spaces ++ x ++ spaces ++ "|"
        edge = ["." ++ replicate (w+2*margin) '-' ++ "."]
        more = replicate margin (fill (replicate w ' '))
        zs = edge ++ more ++ (map fill xs)    
        
----------------------------------------------------
-- iterating over lists of Blocks

lineUp :: [Block] -> Block
lineUp [] = Block 0 0 []
lineUp (t:ts) = beside t (lineUp ts)

stack [] = Block 0 0 []
stack (t:ts) = above t (stack ts)

--sep [] = Block 0 0 []
--sep [x] = x
--sep (x:xs) = beside x (beside (oneBlock "  ") (sep xs))

----------------------------------------------
-- some simple tests

pr (Block w h xs) = putStrLn (concat xs)


b1 = manyBlock ["tim","has many","cats."]
b2 = manyBlock ["Robert has many dogs","But he wilts"]

instance Show Block where
  show (Block w h xs) = (unlines xs)++("\nwidth = "++show w++", height = "++show h++".")

---------------------------------------------------
-- useful helper functions

schema (hyp@(Block w1 h1 xs)) 
       (concl@(Block w2 h2 ys)) 
       name = stack [hyp,line,concl]
  where line = Block (n+length name) 1 ([replicate n '-'++name])
        n = max w1 w2 

sep n xs = lineUp(punc xs) where
  punc [] = []
  punc [x] = [x]
  punc (x:xs) = x: oneBlock (replicate n ' '):(punc xs)

sequentBlock [] x name = x -- lineUp [oneBlock "[",x,oneBlock "]"]
sequentBlock xs x name = schema hyps (center x width) name
  where hyps = (sep 3 xs)
        (width,l) = dim hyps


plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    
  

plist :: Show a => String -> [a] -> String -> String -> String
plist = plistf show
