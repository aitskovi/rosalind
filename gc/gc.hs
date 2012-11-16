import System.IO

data Fasta = Fasta String String
           | Empty


main = interact f
     where
         f = print . gc . parse . lines
         print x = fname x ++ "\n" ++ (show $ percent x) ++ "\n"

gc :: [Fasta] -> Fasta
gc xs = foldr (\x y -> if percent x > percent y then x else y) Empty xs

percent :: (Fractional a) => Fasta -> a
percent Empty = 0
percent (Fasta n d) =  gcCount / total * 100
    where
        gcCount = fromIntegral $ length $ filter (\x -> (x == 'C') || (x == 'G')) d
        total = fromIntegral $ length d

-- Parse a FASTA data file
parse :: [String] -> [Fasta]
parse xs = foldl parseLine [] xs

-- Parse a FASTA data file line
parseLine :: [Fasta] -> String -> [Fasta]
parseLine xs [] = xs
parseLine [] ys = if isName ys then [(parseName ys)] else []
parseLine ((Fasta n d):xs) ys = if isName ys then (parseName ys):((Fasta n d):xs)
                                            else (Fasta n (d ++ ys)):xs
-- Generate a string from a Fasta DataType
stringify :: Fasta -> String
stringify (Fasta name value) = name ++ "\n" ++ value

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
