module Utils.Bioinformatics(Fasta(Fasta, Empty), interactFasta, rna, mrna) where

import System.IO

data Fasta = Fasta String String
           | Empty

interactFasta :: ([Fasta] -> String) -> IO ()
interactFasta f = interact (f . parse . lines)

-- Parse a FASTA data file
parse :: [String] -> [Fasta]
parse xs = reverse $ foldl parseLine [] xs

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

-- Translate a dna string into an RNA string
rna :: [Char] -> [Char]
rna xs = replace 'T' 'U' xs

replace :: Char -> Char -> [Char] -> [Char]
replace a b xs = map (\x -> if x == a then b else x) xs

-- Translate an RNA string into a correspondig mRNA string
mrna :: String -> String
mrna xs = concat $ takeWhile (/= "Stop") $ map amino $ triples xs

triples :: [a] -> [[a]]
triples [] = []
triples xs = y:(triples ys)
    where (y, ys) = splitAt 3 xs

-- Translate Codon to Amino Acid
amino :: String -> String
amino "UUU" = "F"
amino "CUU" = "L"
amino "AUU" = "I"
amino "GUU" = "V"
amino "UUC" = "F"
amino "CUC" = "L"
amino "AUC" = "I"
amino "GUC" = "V"
amino "UUA" = "L"
amino "CUA" = "L"
amino "AUA" = "I"
amino "GUA" = "V"
amino "UUG" = "L"
amino "CUG" = "L"
amino "AUG" = "M"
amino "GUG" = "V"
amino "UCU" = "S"
amino "CCU" = "P"
amino "ACU" = "T"
amino "GCU" = "A"
amino "UCC" = "S"
amino "CCC" = "P"
amino "ACC" = "T"
amino "GCC" = "A"
amino "UCA" = "S"
amino "CCA" = "P"
amino "ACA" = "T"
amino "GCA" = "A"
amino "UCG" = "S"
amino "CCG" = "P"
amino "ACG" = "T"
amino "GCG" = "A"
amino "UAU" = "Y"
amino "CAU" = "H"
amino "AAU" = "N"
amino "GAU" = "D"
amino "UAC" = "Y"
amino "CAC" = "H"
amino "AAC" = "N"
amino "GAC" = "D"
amino "UAA" = "Stop"
amino "CAA" = "Q"
amino "AAA" = "K"
amino "GAA" = "E"
amino "UAG" = "Stop"
amino "CAG" = "Q"
amino "AAG" = "K"
amino "GAG" = "E"
amino "UGU" = "C"
amino "CGU" = "R"
amino "AGU" = "S"
amino "GGU" = "G"
amino "UGC" = "C"
amino "CGC" = "R"
amino "AGC" = "S"
amino "GGC" = "G"
amino "UGA" = "Stop"
amino "CGA" = "R"
amino "AGA" = "R"
amino "GGA" = "G"
amino "UGG" = "W"
amino "CGG" = "R"
amino "AGG" = "R"
amino "GGG" = "G"
