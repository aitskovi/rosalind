import Data.List
import Data.Maybe
import Utils.Bioinformatics

main = interactFasta $ mrna . rna . splc . sequences

splc :: [String] -> [Char] 
splc (f:fs) = foldr (\fst rst -> strip fst rst) f fs
        
sequences :: [Fasta] -> [[Char]]
sequences fs = map sequence fs
    where
        sequence (Fasta n s) = s

-- Strips any occurences of the given subsequence from the list.
strip :: Eq a => [a] -> [a] -> [a]
strip xs [] = []
strip xs (y:ys) = if isNothing stripped then y : strip xs ys else strip xs $ fromJust stripped
    where
        stripped = stripPrefix xs (y:ys)

