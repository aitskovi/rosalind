import Data.Array

main = interact $ unwords . (map show) . deg . lines

deg :: [String] -> [Int]
deg (x:xs) = elems $ accumArray (+) 0 (1,n) $ zip ints [1,1..]
  where
    n : m : [] = map (\x -> read x :: Int) (words x)
    ints = map (\x -> read x :: Int) $ concatMap words xs
