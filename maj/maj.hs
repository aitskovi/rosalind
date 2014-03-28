main = interact $ unwords . (map show) . maj . lines

maj :: [String] -> [Int]
maj (x:xs) = map majority numberLists
  where
    numberLists = map (\ys -> map (\y -> read y :: Int) (words ys)) xs

-- An implemenation of the moore majority algorithm
majority :: (Eq a, Num a) => [a] -> a
majority [] = -1
majority xs = if major then moore else -1
  where
    moore = mooreMajority xs (head xs) 0
    major = length (filter (== moore) xs) > (length xs) `div` 2

mooreMajority :: (Eq a, Eq b, Num b) => [a] -> a -> b -> a
mooreMajority [] y count = y
mooreMajority (x:xs) y count
  | x == y     = mooreMajority xs y (count + 1)
  | count == 0 = mooreMajority xs x 0
  | otherwise  = mooreMajority xs y (count - 1)
