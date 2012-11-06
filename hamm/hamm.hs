hamm :: [Char] -> [Char] -> Int
hamm xs ys = length $ filter (\(x,y) -> x /= y) $ zip xs ys
