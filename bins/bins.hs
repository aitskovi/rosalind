import Data.Array

main = interact $ unwords . (map show) . bins

bins string = map (\x -> binarySearch x haystack 1 n) needles
  where
    _ : _ : xs : ys : _ = lines string
    numbers = map (\x -> read x :: Int) $ words xs
    needles = map (\x -> read x :: Int) $ words ys
    n = length numbers
    haystack = listArray (1, n) numbers

binarySearch x xs lo hi
  | hi < lo           = -1
  | x < xs ! middle   = binarySearch x xs lo (middle - 1)
  | x > xs ! middle   = binarySearch x xs (middle + 1) hi
  | otherwise         = middle
  where
    middle = lo + ((hi - lo) `div` 2)

