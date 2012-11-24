data Fasta = Fasta String String
           | Empty
    deriving Show

grph :: [Fasta] -> [(Fasta, Fasta)]
grph fs = concat $ map (\x -> map (\y -> (x,y)) (filter (match x) fs)) fs

match :: Fasta -> Fasta -> Bool
match (Fasta n1 d1) (Fasta n2 d2) = if d1 == d2 then False else first == last
    where
        first = take 3 d2
        last = drop (length d1 - 3) d1

main = interact f
    where
        f = print . grph . parse . lines
        print x = concat $ map (\(a,b) -> fname a ++ " " ++ fname b ++ "\n") x

-- ------------- --
-- FASTA Parsing --
-- ------------- --

-- Parse a FASTA data file
parse :: [String] -> [Fasta]
parse xs = foldl parseLine [] xs

-- Parse a FASTA data file line
parseLine :: [Fasta] -> String -> [Fasta]
parseLine xs [] = xs
parseLine [] ys = if isName ys then [(parseName ys)] else []
parseLine ((Fasta n d):xs) ys = if isName ys then (parseName ys):((Fasta n d):xs)
                                            else (Fasta n (d ++ ys)):xs
fname :: Fasta -> String
fname Empty = "Empty"
fname (Fasta n d) = n

-- Verify that a string represents a FASTA Name
isName :: String -> Bool
isName [] = False
isName (x:xs) = (x == '>')

-- Parse a FASTA Name
parseName :: String -> Fasta 
parseName (x:xs) = Fasta xs ""
