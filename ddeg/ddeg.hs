import Data.Array

main = interact $ unwords . (map show) . ddeg . lines

ddeg :: [String] -> [Int]
ddeg (nums : xs) = map (\ys -> sum (map (\y -> degrees ! y) ys)) adjacencies
  where
    (n, m) = readPair nums
    pairs = map readPair xs
    adjacencies = adjacent pairs n 
    degrees = deg pairs n

readPair :: String -> (Int, Int)
readPair xs = (read a :: Int, read b :: Int)
  where
    a : b : _ = words xs

deg :: [(Int, Int)] -> Int -> Array Int Int
deg xs n = accumArray (+) 0 (1,n) $ zip ints [1,1..]
  where
    ints = concatMap (\x -> [fst x, snd x]) xs

-- Write adjacency function
adjacent :: [(Int, Int)] -> Int -> [[Int]]
adjacent xs n = elems $ accumArray (\accum x -> x : accum) [] (1, n) edges
  where
    edges = xs ++ pairFlip xs 

pairFlip :: [(a, a)] -> [(a, a)]
pairFlip xs = map (\(a,b) -> (b,a)) xs
