dna :: [Char] -> [Int]
dna ds = map (\x -> count x ds) ['A', 'C', 'G', 'T'] 

count :: Char -> [Char] -> Int
count char ds = length $ filter (== char) ds
