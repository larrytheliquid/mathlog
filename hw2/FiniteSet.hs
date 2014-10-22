-- This file is a simple version of FiniteSet.hs modified
-- from the Funlog system. 

{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, 
    DeriveDataTypeable, FlexibleInstances,
    RankNTypes, GADTs  #-}
module FiniteSet where
import Prop
                    
import qualified Data.Map as DM 
import Data.Map hiding (map,filter,union,difference)
import Data.List(nub,find,elemIndex)
import qualified Data.List as List
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)


import Data.Typeable

-- import Debug.Trace
-- ttx s x = trace (s++show x) x

----------------------------------------------------------------
-- We can make "FiniteSet" for "Base" types. We store elements
-- of these types as "Literal". A "Dim" is a finite set of
-- some "Base" type. We store a list of "Literal" inside
-- representing this set.

data Base = Int | String | Char | Double | Bool | Enum String | Tuple [Base]
  deriving Eq
  
data Literal 
   = LInt { unInt :: Int }         -- 5 , -5
   | LDouble { unDouble::Double }  -- 2.3 , 0.5 , -3.4
   | LChar { unChar::Char }        -- 'x'
   | LString {unString::String}    -- "\nabc"S@
   | LUnit
   | LCon String String        -- True, False, constants of enumerated types 
                               -- i.e. a type where ALL constructors are Nullary!
   deriving (Typeable)                               

-- A Dimension is a finite set of some Base type.
-- Invariant that (Dim n b xs) => length xs == n
-- We store the elements of the set as a [Literal]
-- The order in the list assigns an Int to each element.

data Dimension = Dim Int Base [Literal] 

-- A (FiniteSet a) is mapping from a prefix of the Integers to "a"
-- The prefix counts from [0 .. (product (map dimD dimensions)) - 1] 
-- (see flatInts)
data FiniteSet a = 
     FA [Dimension]       -- The n dimensions.
        (DM.Map Int a)    -- Mapping of Row-major index to value.
        
----------------------------------------------------------------------
-- Dimensions

dimName (Dim n base ls) =  show base++"#"++show n
dimNameAll (Dim n base (l:ls)) = show base++"#"++show(l:ls)
dimSize (Dim n base ls) = n
dimElem (Dim n base ls) = ls

dimNameL ds = plistf dimName "(" ds "," ")"

litToIndex :: Dimension -> Literal -> Int                         
litToIndex (Dim n base xs) a =
   case elemIndex a xs of
     Just n -> n
     Nothing -> error ("The element: "++show a++" can't be found in the index set "++show xs)

indexToLit :: Dimension -> Int -> Literal
indexToLit (d@(Dim n base xs)) a 
   | (a < 0) || (a >= length xs) 
   = error ("Bad indexToLit "++show d++" "++show a)
indexToLit (Dim n base xs) a = xs !! a

-- We can create Dimensions a number of ways.
-- State the size, and we generate a mapping [0 .. n]
dim :: Int -> Dimension
dim n = Dim n Int [LInt x | x <- [0 .. n-1]]

-- Exhibit a list of strings that describes the mapping
dimS:: [String] -> Dimension
dimS xs = Dim (length xs) String (map LString xs)

-- Exhibit a list of anything that can be turned into a string.
dimL :: (Show a) => [a] -> Dimension
dimL xs = Dim (length xs) String (map (LString . show) xs)

-- Any element of a type in the Enum class describes a dimension
-- over the finite set described by the full enumeration.
dimE :: (Show a,Enum a) => String -> a -> Dimension
dimE name x = Dim (length xs) (Enum name) (map (LCon name . show) xs)
  where first = (toEnum 0)
        theyHaveTheSameType = [first,x]
        xs = enumFrom first

-- Create a multi-Dimensional mapping where all the individual 
-- mappings are over a 0-based range of Int.
expand :: [Int] -> [Dimension]
expand xs = map dim xs

-- The listing of all the indices from a multi-dimensional thing
flatInts dims = [0 .. product (map dimSize dims) - 1]

showDimSize (Dim n base xs) = show n
showDimL ds = show(map dimName ds)
  
------------------------------------------------------------------------------------
-- A FiniteSet stores a k-dimensional Array as a 1-dimensional partial sparse array.
-- It maps k indices to 1 index, and uses this as the key in a Data.Map.
-- The array is partial and sparse, because only the known indexes are stored.
-- To compute the indices see flatIndex and kIndex.
-- looking up an unknown index is undefined. 
-- In some instantiations unknown indices will mean a default value and the
-- lookup function should be put in a wrapper function to produce this default.

dimFA (FA ds arr) = ds
sizeFA (r@(FA ds arr)) = product (dims r)
lookupFA indices (FA ds arr) = Data.Map.lookup (flatIndex ds indices) arr 
boundsFA :: FiniteSet t -> (Int, Int)
boundsFA (r@(FA dims arr)) = (0,sizeFA r - 1)
dims:: FiniteSet a -> [Int]
dims (FA ds map) = List.map dimSize ds
countFA (FA ds set) = size set

vars:: Ord a => FiniteSet (Prop a) -> [a]
vars (FA ds s) =  nub $ concat (map (letters . snd) (Data.Map.toList s))

-- flatIndex computes the corresponding 1-dim index from a list of k indices
-- it also needs to know the list of dimensions. This is the Row-Major index

flatIndex:: [Dimension] -> [Int] -> Int
flatIndex dims indices = help 1 (reverse (map dimSize dims)) (reverse indices) 0
  where help m [] [] ans = ans
        help m (d:ds) (x:xs) ans = help (m*d) ds xs (m*x + ans)
     
-- kIndex computes a list of k indices from the dimensions and 
-- corresponding 1-dim index 

kIndex:: [Dimension] -> Int -> [Int]     
kIndex dims m = help m (reverse (map dimSize dims)) []
  where help 0 [] ans = ans
        help 0 (d:ds) ans = help 0 ds (0:ans)
        help n (d:ds) ans = help (n `div` d) ds ((n `mod` d):ans)  
        help n [] ans = error ("The input "++show m++" is too large for the dimensions "++showDimL dims)


litsToKIndex:: [Dimension] -> [Literal] -> Int
litsToKIndex ds lits = flatIndex ds (zipWith litToIndex ds lits)
  
flatIndexToLits :: [Dimension] -> Int -> [Literal]
flatIndexToLits ds n = (zipWith f ds tuple)
  where tuple = kIndex ds n
        f (Dim n base xs) m = xs !! m


-- Use a function to choose if a tuple is present, and if so, what value is stored.                 
partial:: [Dimension] -> ([Int] -> Maybe a) -> FiniteSet a
partial dims pred = 
   FA dims
      (fromList [ (i,j) 
                | i <- flatInts dims 
                , Just j <- [pred (kIndex dims i)] ])        
                
-- Supply a value for every tuple.
universe :: [Dimension] -> ([Int] -> a) -> FiniteSet a
universe dims initf = partial dims (Just . initf)

equal2 :: (Boolean a) => [Dimension] -> FiniteSet a 
equal2 [d1,d2] | d1==d2 = universe [d1,d2] f
  where f [x,y] | x==y = true
        f _ = false
equal2 ds = error "Equal is defined only on 2-Dimensional objects where the dimensions are the same"

equalFA :: (Boolean a) => FiniteSet a -> FiniteSet a
equalFA (FA ds _) = equal2 ds
   
always:: a -> Maybe(Prop Int)
always x = Just(TruthP)
                                              
enumPoly:: ([Int] -> a -> Maybe (a, t)) -> [Dimension] -> a -> (a, FiniteSet t)                     
enumPoly pred dims seed = finish (walk seed (flatInts dims))
  where finish (last,zs) = (last,FA dims (fromList zs))
        walk next [] = (next,[])
        walk next (i:is) = case pred (kIndex dims i) next of 
                             Nothing -> walk next is
                             Just(new,j) -> (final,(i,j):ys)
                                where (final,ys) = walk new is      
                                
enum:: ([Int] -> Int -> Maybe (Int, t)) -> [Dimension] -> Int -> (Int, FiniteSet t)                     
enum = enumPoly                                 

count xs n = Just(n+1,LetterP n)                                
-- (_,six) = enum count [dim 3, dim 2] 1

countWithBounds (lowv,highv) indexes next =
  case (elemFA indexes lowv,elemFA indexes highv) of
   (True,True) -> Just(next,TruthP)
   (False,True) -> Just(next+(1::Int),LetterP next)
   (True,False) -> error ("Inconsistent bounds. The lower bound\n  "++show lowv++"\nShould be a subset of the upperbound\n   "++show highv)
   (False,False) -> Nothing -- Just(next,AbsurdP)

elemFA indexes set = 
  case lookupFA indexes set of
    Nothing -> False
    Just t -> t
    
    
---------------------------------------------------------------------
-- Creating Relations

emptyRel dims = FA dims Data.Map.empty
fullRel dims = universe dims 
                   -- (\ _ -> (TruthP::Prop Int))
                      (\ _ -> true)

-- detect when things are out of range for the dimensions
badElements [] [] = False
badElements (d:ds) (x:xs) = (badElem d x) || (badElements ds xs)
  where badElem dim x = x < 0 || x >= (dimSize dim)
badElements [] (x:xs) = error "To many elements for the dimensions given."
badElements (d:ds) [] = error "To few elements for the dimensions given."
 
-- supply a list of tuples, and make a set from them.
-- every tuple is associated with the same value 'v'

fromIndexList:: t -> [Dimension] -> [[Int]] -> FiniteSet t
fromIndexList v dim xss | any (badElements dim) xss = 
   error ("In 'fromIndexList' one of the elements\n"++show xss++"\nis not appropriate for dimension "++showDimL dim)
fromIndexList v dim xss = FA dim (fromList (map f xss))
   where f xs = (flatIndex dim xs,v)

fromLitList:: v -> [Dimension] -> [Literal] -> FiniteSet v
fromLitList v ds xs | length ds == length xs = FA ds (fromList (zipWith f ds xs))
  where f d x = (litToIndex d x,v)
fromLitList v ds xs = 
  error ("The dimensions "++showDimL ds++
         " are not the right width for the tuple: "++
         show xs++indentL ds)
  
{-
-- Uses Finite class magic to create Sets from lists of objects from a Finite type.
 
fromFiniteList:: (Finite t) => v -> [Dimension] -> [t] -> FiniteSet v
fromFiniteList v ds xs = FA ds (fromList (map f xs))
  where f x = (toIndex ds x,v)
-}


-- Relation is a FiniteSet of (Prop Int)
-- any index entry not populated, is assumed to be "AbsurdP"
-- Make versions of fromIndexList and fromFiniteList specialized to (FiniteSet (Prop Int)) 


type Relation = FiniteSet (Prop Int)  

many::[Dimension] -> [[Int]] -> Relation
many ds xs = fromIndexList true ds xs

manyD:: Finite a => [Dimension] -> [a] -> Relation
manyD ds xs = fromFiniteList true ds xs

manyL::[Dimension] -> [Literal] -> Relation
manyL ds xs = fromLitList true ds xs





                                             
instance Boolean Bool where
  true = True
  false = False
  isTrue x = x
  isFalse = not
  conj = (&&)
  disj = (||)
  neg = not
  imply x y = not x || y
  
instance (PPLetter n,Ord n) => Boolean (Prop n) where
  true = TruthP
  false = AbsurdP
  isTrue TruthP = True
  isTrue x = False
  isFalse AbsurdP = True
  isFalse x = False
  conj = andB
  disj = orB
  neg = notB
  imply = implyB
                            
-------------------------------------------------------------------------
-- In some functions we create new Relations by scanning over the
-- elements in an old relation. We must use special cases to handle the
-- missing indices that are not stored because they are AbsurdP

scanf:: Boolean b => Int -> (Int -> b -> t -> t) -> Int -> [(Int, b)] -> t -> t
scanf max acc n [] ans 
  | n > max = ans
  | True    = scanf max acc (n+1) [] (acc n false ans)
scanf max acc n (ys@((k,v):xs)) ans
  | n==k = scanf max acc (n+1) xs (acc k v ans)
  | n<k  = scanf max acc (n+1) ys (acc n false ans)
  | n>k  = error ("scanf out of step: n="++show n++" next="++show (k,v))  


unary:: (Boolean b1,Boolean b2) => ([Int] -> b1 -> b2) -> FiniteSet b1 -> FiniteSet b2
unary g (r@(FA dims table)) = (FA dims (fromList new))
  where ascendingList = toAscList table
        (first,last) = boundsFA r
        new = scanf last h first ascendingList []
        h n p c = add n (g (kIndex dims n) p) c    

{-        
unaryV:: (Boolean b1,Boolean b2) => ([Value] -> b1 -> b2) -> FiniteSet b1 -> FiniteSet b2
unaryV g (r@(FA dims table)) = (FA dims (fromList new))
  where ascendingList = toAscList table
        (first,last) = boundsFA r
        new = scanf last h first ascendingList []
        h n p c = add n (g (flatIndexToValues dims n) p) c            
-}

-- in the function "add" if a result ends up "false", 
-- throw it away, since it will be 
-- represented by the default value of "false"

add key x ans | isFalse x = ans
add key result ans = (key,result):ans

foldRel (r@(FA dims table)) f = (FA dims (Data.Map.fromList list))
  where ascList = toAscList table
        (first,last) = boundsFA r
        list = scanf last f first ascList []

-------------------------------------------------------------------------
-- tailMerge is a binary scan down two Relations, using special cases to 
-- handle the missing indices that are not stored because they are AbsurdP.

tailMerge add g [] [] ans = ans
tailMerge add g ((k1,e1):xs1) [] ans = tailMerge add g xs1 [] (add k1 (g e1 false) ans)
tailMerge add g [] ((k2,e2):xs2) ans = tailMerge add g [] xs2 (add k2 (g false e2) ans)
tailMerge add g (ys1@((k1,e1):xs1)) (ys2@((k2,e2):xs2)) ans =
  case compare k1 k2 of
    EQ -> tailMerge add g xs1 xs2 (add k1 (g e1 e2) ans)
    LT -> tailMerge add g xs1 ys2 (add k1 (g e1 false) ans)
    GT -> tailMerge add g ys1 xs2 (add k2 (g false e2) ans)

binary::(Boolean a, Boolean b, Boolean c) =>
        (a -> b -> c) -> FiniteSet a -> FiniteSet b -> FiniteSet c
binary g (FA dim1 tab1) (FA dim2 tab2)
   | dim1 /= dim2 = error ("Non matching dimensions in pointwise binary operator\n  "++dimNameL dim1++"\n  "++dimNameL dim2)
binary g (FA dim1 tab1) (FA dim2 tab2) = 
                  (FA dim1 (fromList newAscendL))
  where ascendL1 = toAscList tab1
        ascendL2 = toAscList tab2
        newAscendL = reverse (tailMerge add g ascendL1 ascendL2 [])
 
--------------------------------------------------------------------
-- Simple logical operations on FiniteSets are implemented pointwise
-- and represent the set operations.

complement :: Boolean b => FiniteSet b -> FiniteSet b 
complement = unary (\ n x -> neg x) 

intersect :: Boolean b => FiniteSet b -> FiniteSet b -> FiniteSet b 
intersect x y = binary conj x y

union :: Boolean b => FiniteSet b -> FiniteSet b -> FiniteSet b 
union = binary disj

difference:: Boolean b => FiniteSet b -> FiniteSet b -> FiniteSet b 
difference x y = intersect x (complement y)


data SymSet b 
  = Srel String (FiniteSet b)
  | Scomplement (SymSet b)
  | Sintersect (SymSet b) (SymSet b)
  | Sunion (SymSet b) (SymSet b)
  | Sdifference (SymSet b) (SymSet b)
  | Sproject [Int] (SymSet b)
  | Sselect ([Int] -> Bool) (SymSet b)
  | Sjoin Int (SymSet b) (SymSet b)

ppSS :: SymSet b -> Doc
ppSS (Srel s x) = text s
ppSS (Scomplement x) = PP.parens (text "Not" <>ppSS x <> text ")")
ppSS (Sintersect x y) = PP.parens $ PP.sep[text "Inter",ppSS x,ppSS y]
ppSS (a@(Sunion _ _)) = PP.vcat [text "Union",PP.nest 3 $ PP.vcat(collect a)]
  where collect (Sunion x y) = collect x ++ collect y
        collect x = [ppSS x]
ppSS (Sdifference x y) = PP.parens $ PP.sep[text "Diff",ppSS x,ppSS y]
ppSS (Sjoin n  x y) = PP.parens $ PP.sep[text "Join",text(show n),ppSS x,ppSS y]
ppSS (Sproject xs x) = PP.parens $ PP.sep[text "Proj",text(show xs),ppSS x]
ppSS (Sselect f x) = PP.parens $ PP.sep[text "Select",text "?" ,ppSS x]

instance Show (SymSet b) where
  show x = render(ppSS x)
  
symToFinite:: Boolean b => SymSet b -> FiniteSet b
symToFinite (Srel s x) = x
symToFinite (Scomplement x) = complement(symToFinite x)
symToFinite (Sintersect x y) = intersect (symToFinite x) (symToFinite y)
symToFinite (Sunion x y) = union (symToFinite x) (symToFinite y)
symToFinite (Sdifference x y) = difference (symToFinite x) (symToFinite y)
symToFinite (Sjoin n x y) = join n (symToFinite x) (symToFinite y)
symToFinite (Sproject xs x) = project xs (symToFinite x)
symToFinite (Sselect f x) = select f (symToFinite x)


trSym:: Boolean b => SymSet b -> SymSet b
trSym (Srel s x) = Srel s x
trSym (Scomplement x) = Scomplement(trSym x)
trSym (Sintersect x y) = Sintersect (trSym x) (trSym y)
trSym (Sunion x y) = Sunion (trSym x) (trSym y)
trSym (Sdifference x y) = Sdifference (trSym x) (trSym y)
trSym (Sjoin n x y) = 
  case (trSym x,trSym y) of
    (Sunion a b,Sunion c d) -> foldr1 Sunion [Sjoin n a c,Sjoin n a d, Sjoin n b d, Sjoin n b c]
    (Sunion a b,c) -> Sunion (Sjoin n a c) (Sjoin n b c)
    (a,Sunion b c) -> Sunion (Sjoin n a b) (Sjoin n a c)
    (x,y) -> Sjoin n x y
trSym (Sproject xs x) = 
  case trSym x of
    Sunion a b -> Sunion (trSym(Sproject xs a)) (trSym(Sproject xs b))
    ans -> Sproject xs ans
trSym (Sselect f x) = Sselect f (trSym x)
  

-----------------------------------------------------------------
-- Relational operations:  join, select, and project 

project :: (Boolean a) => [Int] -> FiniteSet a -> FiniteSet a
project cols (r@(FA dims table)) | any bad cols = error ("Bad projection columns "++show cols++" don't match with dimensions "++showDimL dims)
  where bad x = x < 0 || x >= length dims
project cols (r@(FA dims table)) | cols == [0 .. length dims - 1] = r
project cols (r@(FA dims table)) = (FA newdims tab3)
  where newdims = fmap (dims !!) cols
        tab2 = toList table
        tab3 = fromListWith disj (scan tab2)
        scan [] = []
        scan ((key,bool):xs) = (newflatkey,bool):scan xs
                  where tupkey = kIndex dims key
                        newtupkey = fmap (tupkey !!) cols
                        newflatkey = flatIndex newdims newtupkey

-------------------------------------------------------------------                                               


select:: (Boolean t) => ([Int] -> Bool) -> FiniteSet t -> FiniteSet t                                                              
select testp (r@(FA dims tab)) = foldRel r acc
  where acc flatkey b ans = case (testp (kIndex dims flatkey),b) of
                             (True,x) | isFalse x -> ans
                             (True,b) -> (flatkey,b):ans
                             other -> ans                       


-------------------------------------------------------------

join:: (Boolean a) => Int -> FiniteSet a -> FiniteSet a -> FiniteSet a
join n (FA dim1 tab1) (FA dim2 tab2) | take n dim1 /= take n dim2 = error "Bad join"
join n (FA dim1 tab1) (FA dim2 tab2) = (FA dim3 tab3)       
  where tab3 = fromListWith disj (joinScan l1 l2)
        dim3 = dim1 ++ drop n dim2
        l1 = toAscList tab1
        l2 = toAscList tab2
        joinScan (xs@((k1,v1):ms)) (ys@((k2,v2):ns)) =
          case (matching n tupkey1 tupkey2) of
            EQ -> let (prefix1,suffix1) = splitR dim1 (matching n tupkey1) xs
                      (prefix2,suffix2) = splitR dim2 (matching n tupkey2) ys
                      pairs = [ (flatIndex dim3 (tupk1 ++ drop n tupk2),conj v1 v2)   
                              | (tupk1,v1) <- prefix1
                              , (tupk2,v2) <- prefix2 ]
                  in pairs ++ joinScan suffix1 suffix2
            LT -> joinScan ms ys
            GT -> joinScan xs ns
          where (tupkey1@(i:is)) = kIndex dim1 k1
                (tupkey2@(j:js)) = kIndex dim2 k2
        joinScan _ _ = []        

matching 0 xs ys = EQ
matching 1 (x:xs) (y:ys) = compare x y
matching n (x:xs) (y:ys) | x==y = matching (n-1) xs ys
                         | otherwise = compare x y
matching _ [] (y:ys) = LT
matching _ (x:xs) [] = GT
matching _ [] [] = EQ

splitR dims matches [] = ([],[])
splitR dims matches (ys@((key,v):xs)) =
       case matches tupKey of
          EQ  -> let (matching,others) = splitR dims matches xs
                 in ((tupKey,v):matching,others)
          otherwise ->  ([],ys)
   where tupKey = kIndex dims key

-----------------------------------------------------------------------
-- Compute if some property is true on a Finite Set.

forall:: (Boolean b) => FiniteSet t -> (FiniteSet t -> b) -> b
forall (FA dim tab) f = List.foldr acc true l1
  where l1 = toAscList tab
        acc (key,x) y = conj (f (FA dim (fromList [(key,x)]))) y

some :: (Boolean b) => FiniteSet b -> b
some (FA dim1 tab1) = List.foldr acc false l1
  where l1 = toAscList tab1
        acc (key,x) y = disj x y

none :: (Boolean b) => FiniteSet b -> b
none (FA dim1 tab1) = List.foldr acc true l1
  where l1 = toAscList tab1
        acc (key,x) y = conj (neg x) y        

full :: Boolean b => FiniteSet b -> b
full (r@(FA dims table)) = scanf last acc first ascendingList true
  where ascendingList = toAscList table
        (first,last) = boundsFA r
        acc n x y = conj x y



-- "one" can be quite expensive in that it creates large Propositions

one :: (Boolean t) => FiniteSet t -> t
one (FA dim1 tab1) = oneFromList (toAscList tab1)

oneFromList xs = 
  List.foldr disj false
        [ List.foldr conj true [ if i==j then b else neg c | (j,c) <- xs ] 
        | (i,b) <- xs ]   
        
onetests = [(i::Int,LetterP i) | i <- [1..4]]        

-- Is one Finite Set a subset of another Finite Set?

subset:: (Boolean t) => FiniteSet t -> FiniteSet t -> t
subset (FA dim1 tab1) (FA dim2 tab2) | dim1 /= dim2 = error ("subset dimension mismatch\n  "++show dim1++ "\n   =/=\n  " ++ show dim2)
subset (FA dim1 tab1) (FA dim2 tab2) = subsetL (toAscList tab1) (toAscList tab2)

subsetL:: (Boolean b, Ord t) => [(t, b)] -> [(t, b)] -> b
subsetL xs ys = tailMerge add g xs ys true
  where g x y | isFalse x = true
        g x y | isFalse y = neg x
        g x y = imply x y -- disj (conj x y) (neg x)
        add key y x | isFalse y = false
        add key x y  = conj x y

{-
-----------------------------------------
-- Creating non-standard functions

constToV :: (forall b . Boolean b => FiniteSet b -> b) -> Value
constToV f = VFun g
  where g (VSet BoolI set) = to (f set)
        g (VSet PropI set) =  VNS(NSProp (f set)) 
        g x = error ("Bad input to 'full'\n   "++show x)

applyV :: (forall b . Boolean b => FiniteSet b -> FiniteSet b) -> Value -> Value
applyV f (VSet BoolI set) = VSet BoolI (f set)
applyV f (VSet PropI set) = VSet PropI (f set)
applyV f x = error ("Bad input to 'applyV'\n   "++show x)

fullV = constToV full
someV = constToV some
noneV = constToV none
oneV = constToV FiniteSet.one
projV xs = applyV (project xs)

subSetV = VFun g
  where g (VSet BoolI set1) = VFun (h set1)
        g (VSet PropI set1) = VFun (f set1)  
        g x = error ("Bad first input to 'subset'\n   "++show x)
        h :: FiniteSet Bool -> Value -> Value
        h set1 (VSet BoolI set2) = to (subset set1 set2)
        h set1 (VSet PropI set2) = VNS(NSProp (subset (liftFiniteSet set1) set2))
        h set1 x = error ("Bad second input to 'subset'\n   "++show x)
        f :: FiniteSet (Prop Int) -> Value -> Value
        f set1 (VSet PropI set2) = VNS(NSProp (subset set1 set2 ))
        f set1 (VSet BoolI set2) = VNS(NSProp (subset set1 (liftFiniteSet set2)))
        f set1 x = error ("Bad second input to 'subset'\n   "++show x)               
-}

liftFiniteSet :: FiniteSet Bool -> FiniteSet (Prop Int)  
liftFiniteSet x = unary f x
  where f _ True = true
        f _ False = false
-----------------------------------------------------------------------
-- functional dependency can be expressed as a self join and then a 
-- selection of tuples where the range columns don't match. If
-- that relation is empty (the 'none' predicate) then there is 
-- a functional dependency. (funDep dom rng)   means that the
-- columns 'dom' functional determine the columns 'rng'
-- I.e.
-- funDep [0,1] [2] x = none(select (colsDiff [(2,3)]) 
--                                  (join 2 (project [0,1,2] x)(project [0,1,2] x)))
-- Note that the selfjoin has columns [0,1,2,2]  
--                      with indices   0,1,2,3 , so we need columns (2,3) not to match
-- Another example
-- funDep [1] [2,0] x = none(select (colsDiff [(1,3),(2,4)]) 
--                                  (join 1 (project [1,2,0] x)(project [1,2,0] x)))
-- Note that the selfjoin has columns [0,2,0,2,0]  
--                      with indices   0,1,2,3,4 so we need columns (1,3) and (2,4) not to match


bad dims x = (x < 0 || x >= length dims) 

funDep:: (Boolean b) => [Int] -> [Int] -> FiniteSet b -> b
funDep dom rng (r@(FA dims table)) | any (bad dims) dom  = 
     error ("Domain indices "++show dom++" not in dimension range 0.."++show (length dims -1))
funDep dom rng (r@(FA dims table)) | any (bad dims) rng = 
     error ("Range indices "++show rng++" not in dimension range 0.."++show (length dims -1))            
funDep dom rng r | not(List.null(List.intersect dom rng)) =
     error ("Domain "++show dom++" and Range "++show rng++" are not disjoint.")
funDep dom rng (r@(FA dims table)) = none(select (not . colsSame rngIndices) (join (length dom) r2 r2))
  where r2 = project (dom++rng) r
        rngIndices = [(length dom + i, length dom + length rng + i) | i <- [0..length rng - 1]]
        -- colsDiff [] xs = False
        -- colsDiff ((i,j):more) xs = (xs!!i)/=(xs!!j) || colsDiff more xs
        colsSame [] xs = True
        colsSame ((i,j):more) xs = (xs!!i)==(xs!!j) && colsSame more xs
{-
funDepV = VFun f
  where f u | Just vs <- isIntList u = VFun (g vs)
        f u = error ("Domain in funDep is not [Int]: "++show u)
        g vs u | Just us <- isIntList u = VFun (h vs us)
        g vs u = error ("Range in funDep is not [Int]: "++show u) 
        h vs us (_) = undefined
-}        
-----------------------------------------------------------------
-- simple data to test things with


x1,x2:: Relation
x1 = partial (expand[3,3]) (\ xs -> if sum xs == 3 then Just(LetterP 3) else Nothing)
x2 = partial (expand[3,3]) (\ xs -> if sum xs == 2 then Just TruthP else Nothing)


props = [p3,OrP p1 p5,TruthP,p4,NotP p3,p4,AbsurdP,NotP (AndP p4 p5),AndP p2 p7]

plus1 = many (expand [10,10]) [[i,i+1] | i <- [0..8]]
minus1 = project [1,0] plus1
plus2 = project [2,1] (join 1 plus1 minus1)
plusEvens = select (\[x,y]-> even x) plus1
close = union plus1 minus1
others = complement plus1

full4By4 = partial (expand[4,4]) always

(next,triples) = enum f (expand[4,4,4]) (99::Int)
   where f [i,j,k] n = if even i then Just(n+1,LetterP n) else Nothing
zeroth = project [0] triples
first = project [1] triples
col12 = project [1,2] triples



----------------------------------------------------------------------
-- evaluation of Propositions using valuation functions in the 
-- form returned by minisat, a list of Int where a negative number
-- means a negated literal.

-- eval2 :: (Eq a) => (Int -> Prop a) -> Prop Int -> [Int] -> Prop a
eval2 f (AndP x y) assign     = andOpt (eval2 f x assign) (eval2 f y assign)   
eval2 f (OrP x y) assign      = orOpt (eval2 f x assign) (eval2 f y assign)   
eval2 f (ImpliesP x y) assign = ImpliesP (eval2 f x assign) (eval2 f y assign)  
eval2 f (NotP x) assign       = notB (eval2 f x assign)
eval2 f TruthP assign         = TruthP
eval2 f AbsurdP assign        = AbsurdP
eval2 f (LetterP x) assign    = case find ok assign of { Nothing -> f x; Just x -> sign x}
  where ok y = abs y == x
        sign x | x <= 0 = AbsurdP
               |x > 0   = TruthP              

instantiate assign r = unary f r
  where f n x = eval2 LetterP x assign  

instan:: [Int] -> FiniteSet (Prop Int) -> FiniteSet Bool
instan minisatSol r = unary f r
  where subst = map toPair minisatSol
        sign x | x <= 0 = False
               |x > 0   = True
        toPair x = (abs x,sign x)
        f n x = eval subst True x
  
------------------------------------------------------------
-- pretty printing
{-
ppFA:: PP a => ([Doc] -> Doc) -> FiniteSet a -> Doc
ppFA cat (FA dims map) = cat (fmap widen (toAscList map))
  where widen (k,x) = toDoc(fromIndex dims k,pp x)
        toDoc (is,str) = text is <> text "=" <> str

vFA xs = ppFA PP.vcat xs
hFA xs = ppFA f xs
  where f xs = PP.braces(commafy xs)

ppDim :: [Int] -> Doc
ppDim xs = PP.parens(commafy (fmap (text . show) xs))

commafy [] = PP.empty
commafy [x] = x
commafy (x:xs) = x <> text "," <> commafy xs

instance PP a => PP (FiniteSet a) where
  pp x = (hFA x)
  
instance PP a => Show (FiniteSet a) where
  show (x@(FA dims tab)) = if Data.Map.null tab then "{}" else render(vFA x)  
-}

ds = expand [5] ++[Dim 2 Bool [LCon "Bool" "False",LCon "Bool" "True"]]

-- pairs = manyD [dim 5,dimE "Bool" True] [(3::Int,True),(4,False),(0,True),(1,False)]  
  
------------------------------------------------------------------
-- Class magic for building Finite Sets from Finite elements 
-- or Tuples of Finite elements. See the function manyD
--------------------------------------------------------

class (Typeable a,Read a,Show a) => Finite a where
  toLit:: a -> [Literal]
  toLit x = [toLiteral x]
  fromLit:: [Literal] -> a
  fromLit [LCon _ x] = read x
  toIndex :: [Dimension] -> a -> Int 
  toIndex [d] a = litToIndex d x where [x] = (toLit a) -- a default implementation
-- Most types except tuples can use the default interpretation of toLit & toIndex
-- Only enumerations, t, in which are instances of (Read t) and (Show  t)
-- should use the default interpretations of fromLit 

-- operations between Base types and Literals

baseType :: Typeable t => t -> Base
baseType x = repToBase(typeOf x)

repToBase rep = 
     case (show con,args) of
       ("Int",[]) -> Int
       ("Char",[]) -> Char
       ("Double",[]) -> Double
       ("Bool",[]) -> Bool
       ("()",[]) -> Tuple[]
       ("(,)",[x,y]) -> Tuple[repToBase x,repToBase y]
       ("(,,)",[x,y,z]) -> Tuple[repToBase x,repToBase y,repToBase z]
       ("(,,,)",[w,x,y,z]) -> Tuple[repToBase w,repToBase x,repToBase y,repToBase z]
       ("(,,,,)",[v,w,x,y,z]) -> Tuple[repToBase v,repToBase w,repToBase x,repToBase y,repToBase z] 
       (name,args)
          | ((show rep)=="[Char]") && (length args == 1) -> String
          | List.null args -> Enum name
          | True -> error ("No base type for: "++show rep)
  where (con,args) = splitTyConApp rep
  
toLiteral:: (Show t,Typeable t) => t -> Literal
toLiteral x = tryAll [try LInt, try useInteger, try LDouble, try LChar, try LString, try un] x
  where un () = LUnit
        useInteger :: Integer -> Literal
        useInteger n = LInt (fromIntegral n)

tname x = show(fst(splitTyConApp (typeOf x))) 
        
tryAll [] x = LCon (tname x) (show x)
tryAll (f:fs) x = 
   case f x of
     Just y -> y
     Nothing -> tryAll fs x
             
try f x = case cast x of
           Just i -> Just (f i)
           _ ->  Nothing   

-- Finite instances
  
instance Read Literal where  
instance Finite [Literal] where
  toLit x = x
  fromLit x = x
  
instance Finite Int where  
  toLit x = [LInt x]
  fromLit [LInt x] = x
instance Finite String where
  toLit x = [LString x]
  fromLit [LString x] = x
instance Finite Double where  
  toLit x = [LDouble x]
  fromLit [LDouble x] = x
instance Finite Char where  
  toLit x = [LChar x]
  fromLit [LChar x] = x  
instance Finite Bool where 
  
-- Tuples are Finite if their components are
instance (Finite a,Finite b) => Finite (a,b) where
  toIndex (ds@[d1,d2]) (a,b) = flatIndex ds [toIndex [d1] a,toIndex [d2] b]
  toIndex ds a = error ("The dimensions "++showDimL ds++
                        " are not appropriate for the tuple: "++
                        show a)
  toLit (x,y) = toLit x ++ toLit y
  fromLit [x,y] = (fromLit [x],fromLit [y])

instance (Finite a,Finite b,Finite c) => Finite (a,b,c) where
  toIndex (ds@[d1,d2,d3]) (a,b,c) = flatIndex ds [toIndex [d1] a,toIndex [d2] b,toIndex [d3] c]
  toIndex ds a = error ("The dimensions "++showDimL ds++
                        " are not appropriate for the tuple: "++
                        show a)
  toLit (x,y,z) = toLit x ++ toLit y ++ toLit z
  fromLit [x,y,z] = (fromLit [x],fromLit [y],fromLit [z])  

                     
fromFiniteList ::
  Finite a => b -> [Dimension] -> [a] -> FiniteSet b
fromFiniteList true ds xs = FA ds (fromList (map f xs))
  where f t = (flatIndex ds (zipWith litToIndex ds (toLit t)),true)

fromFList :: Finite a => [Dimension] -> [(a,b)] -> FiniteSet b
fromFList ds xs = FA ds (fromList (map f xs))
   where f (t,x) = (flatIndex ds (zipWith litToIndex ds (toLit t)),x)

                      
-- helper functions
indentL xs = concat(map f xs)
  where f x = "\n   "++dimName x

findElem [d] a = dimFind d (LString(show a))
findElem ds a = error ("The dimensions "++showDimL ds++
                        " are too wide for 1-Dim element: "++
                        show a++indentL ds)
dimFind (Dim n base xs) a =
   case elemIndex a xs of
     Just n -> n
     Nothing -> error ("The element: "++show a++" can't be found in the index set "++
                       plistf show "[" xs "," "]")

-- The inverse of toIndex is just an overloaded function
fromIndex :: [Dimension] -> Int -> String
fromIndex ds n = showL (zipWith f ds tuple)
  where tuple = kIndex ds n
        f (Dim n base xs) m = show(xs !! m)
        showL [] = "()"
        showL [x] = x
        showL xs = "("++help xs++")"
        help [] = ""
        help [x] = x
        help (x:xs)= x++","++help xs

----------------------------------------------------------  
-- Some class definitions and some instances 
-- Something acts like a Boolean if it supports these
-- operations

class Show b => Boolean b where
  true :: b
  false :: b
  isTrue :: b -> Bool
  isFalse :: b -> Bool
  conj:: b -> b -> b     -- conjunction
  disj:: b -> b -> b     -- disjunction
  neg:: b -> b           -- negation
  imply:: b -> b -> b    -- implication
  
instance Eq Literal where
  (LInt n) == (LInt m) = n==m
  (LDouble n) == (LDouble m) = n==m
  (LChar n) == (LChar m) = n==m
  (LString n) == (LString m) = n==m
  (LCon t x) == (LCon s y) = x==y  
  LUnit == LUnit = True
  _ == _ = False

instance Eq Dimension where
  (Dim n b1 xs) == (Dim m b2 ys) = n==m && b1==b2 && xs==ys
                                                                                                                            
instance Eq a => Eq(FiniteSet a) where
  (FA ds1 m1) == (FA ds2 m2) =  DM.size m1 == DM.size m2 && ds1==ds2 && m1==m2

-- Show instances 
instance Show Dimension where
  show x = dimNameAll x
  
instance Show Base where
  show Int = "Int"
  show Char = "Char"
  show Double = "Double"
  show String = "String"
  show Bool = "Bool"
  show (Enum s) = s
  show (Tuple xs) = plistf show "#(" xs "," ")"

instance Show Literal where
  show (LInt n) = show n
  show (LDouble n) = show n
  show (LChar c) = show c
  show (LString s) = ['"']++s++['"']
  show (LCon tnm name) = name
  show LUnit = "()"
  
plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    
  
instance PP Int where
  pp x = PP.int x
instance PP String where
  pp x = text x
instance PP Char where
  pp x = PP.char x

ppFA :: PP a => (Doc -> Doc) -> ([Doc] -> a1) -> FiniteSet a -> a1
ppFA g cat (FA [] map) =
  case (DM.toAscList map) of
    [(i,x)] -> cat [text "Scalar ",pp x]
    other -> error ("Badly formed zero dimensional table")
ppFA g cat (FA dims map) = cat (text (dimNameL dims) :(fmap widen (DM.toAscList map)))
  where widen (k,x) = -- (text ("KEY "++show k++show(kIndex dims k))) <+> 
                         toDoc(flatIndexToLits dims k,pp x)
        toDoc (is,str) = text (plistf show "(" is "," ")") <> g str
        flatIndexToLits ds n = (zipWith f ds tuple)
          where tuple = kIndex ds n
                f (Dim n base xs) m = xs !! m
        kIndex:: [Dimension] -> Int -> [Int]     
        kIndex dims m = help m (reverse (fmap (\(Dim n base xs)->n) dims)) []
          where help 0 [] ans = ans
                help 0 (d:ds) ans = help 0 ds (0:ans)
                help n (d:ds) ans = help (n `div` d) ds ((n `mod` d):ans)  
                help n [] ans = error ("The input "++show m++" is too large for the dimensions "++ dimNameL dims)               

ppDim :: [Int] -> Doc
ppDim xs = PP.parens(commafy (fmap (text . show) xs))

commafy [] = PP.empty
commafy [x] = x
commafy (x:xs) = x <> text "," <> commafy xs

instance PP (FiniteSet Bool) where
  pp (x@(FA dims tab)) = if DM.null tab then text "{}" else (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = PP.empty

instance PP (FiniteSet (Prop Int)) where
  pp (x@(FA dims tab)) = if DM.null tab then text "{}" else (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x         
 
instance Show (FiniteSet Bool) where
  show (x@(FA dims tab)) = render(pp x)

instance Show (FiniteSet (Prop Int)) where
  show (x@(FA dims tab)) = render(pp x)
  
instance Show (FiniteSet Int) where
  show (x@(FA dims tab)) = if DM.null tab then "{}" else render (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x   
          
instance Show (FiniteSet Char) where
  show (x@(FA dims tab)) = if DM.null tab then "{}" else render (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x             
 
--------------------------------------------------------------------

tim = 45

compareFS f (FA d1 m1) (FA d2 m2) | d1==d2 = zipWith f (toList m1) (toList m2)
compareFS f _ _ = error "Dimensions in compareFS not the same"

pairsFA (FA ds m) = map f (toList m)
  where f (k,x) = (fromIndex ds k,x)




