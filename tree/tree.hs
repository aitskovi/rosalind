import Data.List
import Data.Maybe

-- Implementation for a general graph.
-- Solves the number of components question.
graph :: (Eq a) => [[a]] -> Int
graph xs = length (join xs) - 1

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

-- main = interact $ show . graph . input . lines

-- Implementation for a tree
-- In a tree n - m = 1. So n - m - 1 is the number of missing edges.
-- Where n = num nodes and m = num edges

tree :: [String] -> Int
tree (x:xs) = n - (length xs) - 1
    where
        n = (read x)::Int

main = interact $ show . tree . lines
