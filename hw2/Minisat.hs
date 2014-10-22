module Minisat(putClauses,solveClauses,negatedCl
              ,solveWithMinisat,cycleMinisat,minisat) where

import Prop 
-- import CNF
import System.IO(IOMode(..),openFile,hClose,hPutStrLn)
import System.IO(fixIO,hClose,hFlush,stdout,stdin,hGetLine)
import System.Cmd(rawSystem,system)

readLine prompt = (do {putStr prompt; hFlush stdout; hGetLine stdin})

-- YOU MUST CHANGE THIS SO IT POINTS TO MINISAT ON YOUR MACHINE.

minisat = "C:\\cygwin\\bin\\minisat.exe"

------------------------------------------------------------------
-- This function writes a [[Prop a]] representing a formula
-- in CNF to a file in a format that can be read by the minisat
-- SAT-solver. The [[Prop a]] must contain only LetterP and Negated LetterP


-- putClauses:: (Ord a, Show a,Num a) => FilePath -> [[Prop a]] -> IO ()
putClauses file xs =
  let vars xs = (maximum0 . concat . concat) (map (map letters) xs)
      showClause xs = f xs
        where f [] = " 0"
              f [x] = showLit x++" 0"
              f (x:xs) = showLit x++" "++f xs
      maximum0 [] = 1
      maximum0 xs = maximum xs
      showLit (LetterP x) = show x
      showLit (NotP(LetterP x)) = "-"++show x
      showLit v = error ("Nonliteral in showLit:" ++ show v)
  in do { h <- openFile file WriteMode
        ; hPutStrLn h ("p cnf "++show (vars xs)++" "++show (length xs))
        ; mapM_ (hPutStrLn h . showClause) xs
        ; hClose h }

readMinisatResult:: FilePath -> IO (Maybe [Int])
readMinisatResult resultfile =
  do { result <- readFile resultfile
     ; case lines result of
        "SAT" : xs : _ -> do
           return (Just (takeWhile (/= 0) (map read(words xs))))
        _ -> return $ Nothing
     }

solveClauses:: FilePath -> FilePath -> [[Prop Int]] -> IO (Maybe [Int])
solveClauses cnfName solName clauses = 
  do { putClauses cnfName clauses
     ; rawSystem minisat [cnfName,solName]
     ; readMinisatResult solName }

solveWithMinisat:: FilePath -> FilePath -> [Prop Int] -> IO (Maybe [Int])
solveWithMinisat cnfName solName constraints =
  do { let clauses = concat (map cnf constraints)
     ; solveClauses cnfName solName clauses }

               
cycleMinisat:: a -> ([Int] -> IO a) -> FilePath -> FilePath -> [Prop Int] -> IO a
cycleMinisat defvalue action cnfName solName constraints =
  do { let clauses = concat (map cnf constraints)
     ; ans <- solveClauses cnfName solName clauses
     ; loop 1 defvalue action cnfName solName clauses ans
     }
     
loop n defvalue action cnfName solName clauses Nothing = 
  do { putStrLn "No more solutions"; return defvalue }
loop n defvalue action cnfName solName clauses (Just sol) = 
  do { putStrLn ("Solution "++show n)
     ; ans <- action sol
     ; s <- readLine ("More solutions? <return> for more")
     ; case s of
         ("") -> do { let newcl = (negatedCl sol : clauses)
                       -- ; putStrLn("NEW=\n"++show (head newcl))
                       ; ans <- solveClauses cnfName solName newcl
                       ; loop (n+1) defvalue action cnfName solName newcl ans }     
         ('q':_) -> do { putStrLn "Stopped by user."; return ans}
         other -> do { putStrLn ("Unknown command: "++show other); return ans}
         }


negatedCl :: [Int] -> [Prop Int]
negatedCl xs = map f xs
  where f x | x<0 = (LetterP (abs x))
        f x = NotP(LetterP (abs x))
 