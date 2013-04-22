bunnies :: Int -> [Int]
bunnies k = 0 : 1 : rest (bunnies k)
    where
        rest (x:xs) = k * x + (head xs) : rest xs

bunny n k = bunnies k !! n
