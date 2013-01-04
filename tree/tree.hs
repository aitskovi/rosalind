import Data.List
import Data.Maybe

tree :: (Eq a) => [[a]] -> Int
tree xs = length (join xs) - 1

-- Returns a list of sets that are disjoint.
-- Any two sets that intersect are joined.
join ::(Eq a) => [[a]] -> [[a]]
join [] = []
join [[]] = []
join (x:xs) = if disjoint then x:(join xs) else join updated
    where
        disjoint = length (filter (\y -> length (intersect x y) > 0) xs) == 0
        updated = map (\y -> if length (intersect x y) > 0 then nub (union x y) else y) xs


input :: [String] -> [[Int]]
input (x:xs) = map (:[]) [1..n] ++ map (\x -> map read (words x)) xs
    where
        n = (read x)::Int

main = interact $ show . tree . input . lines
