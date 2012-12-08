module Utils.Bioinformatics(Fasta(Fasta, Empty), interactFasta) where

import System.IO

data Fasta = Fasta String String
           | Empty

interactFasta :: ([Fasta] -> String) -> IO ()
interactFasta f = interact (f . parse . lines)

-- Parse a FASTA data file
parse :: [String] -> [Fasta]
parse xs = foldl parseLine [] xs

-- Parse a FASTA data file line
parseLine :: [Fasta] -> String -> [Fasta]
parseLine xs [] = xs
parseLine [] ys = if isName ys then [(parseName ys)] else []
parseLine ((Fasta n d):xs) ys = if isName ys then (parseName ys):((Fasta n d):xs)
                                            else (Fasta n (d ++ ys)):xs
-- Verify that a string represents a FASTA Name
isName :: String -> Bool
isName [] = False
isName (x:xs) = (x == '>')

-- Parse a FASTA Name
parseName :: String -> Fasta 
parseName (x:xs) = Fasta xs ""
