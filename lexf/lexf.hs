import Data.List

lexf :: Int -> [a] -> [[a]]
lexf n xs = combination n xs xs

combination :: Int -> [a] -> [a] -> [[a]]
combination 0 xs ys = [[]]
combination n (x:[]) ys = (map (x:) (combination (n - 1) ys ys))
combination n (x:xs) ys = (map (x:) (combination (n - 1) ys ys)) ++ (combination n xs ys)

stringify :: [String] -> String
stringify xs = concat $ map (\x -> x ++ "\n") xs
