import Data.List

main = interact seto

seto input = unlines $ map intSetToString results
    where
        inputLines = lines input
        n = (read $ inputLines !! 0)::Int
        a = stringToIntSet $ inputLines !! 1
        b = stringToIntSet $ inputLines !! 2
        universe = [1..n]
        results = [union a b, intersect a b, a \\ b, b \\ a, universe \\ a, universe \\ b]

stringToIntSet :: String -> [Int] 
stringToIntSet input = map read $ words $ strip ',' $ strip '}' $ strip '{' $ input

intSetToString :: [Int] -> String
intSetToString set = "{" ++ items ++ "}"
    where
        string = map show set
        items = join ", " string

strip :: Char -> String -> String
strip char string = filter (/= char) string

join :: String -> [String] -> String
join as [] = []
join as [b] = b
join as (b:bs) = b ++ as ++ join as bs
