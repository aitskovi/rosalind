import Data.List
import Utils.Bioinformatics

type Profile = [[Int]]

nucleobases = ['A', 'C', 'G', 'T']

main = interactFasta $ print . cons
    where
        print (string, ps) = string ++ "\n" ++ (showProfile ps)

showProfile :: Profile -> String
showProfile ps = unlines $ map showProfileLine tupled
    where
        tupled = zip nucleobases ps 
        showProfileLine (base, p) = base : ":" ++ (concat $ map (\x -> (" " ++ show x)) p)

-- Generate a profile matrix and a consensus string 
cons :: [Fasta] -> (String, Profile) 
cons fs = (string, matrix)
    where
        matrix = profile fs
        string = consensus matrix 

-- Generate a consensus string from a profile matrix
consensus :: Profile -> String
consensus ps = map snd paired
    where
        rows = transpose ps
        paired = map (\row -> maximum (zip row nucleobases)) rows

-- Generate a profile matrix from a list of Fasta strings.
profile :: [Fasta] -> Profile
profile [] = []
profile fs = transpose counts
    where
        rows = dna fs
        columns = transpose rows
        counts = map countNucleobases columns

-- Count the occurences of each nucleobase
countNucleobases :: String -> [Int]
countNucleobases string = map (\base -> count base string) nucleobases

-- Count the number of equal elements in the list
count :: (Eq a) => a -> [a] -> Int
count x xs = length $ filter (==x) xs

-- Seperate the dna from Fasta list
dna :: [Fasta] -> [[Char]]
dna [] = []
dna ((Fasta n d):fs) = d:(dna fs)
