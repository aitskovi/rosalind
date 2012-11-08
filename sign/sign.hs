import Data.Char

perms :: [Int] -> [[Int]]
perms [] = [[]]
perms (x:xs) = concat (map (concatall x) (perms xs))

concatall :: Int -> [Int] -> [[Int]]
concatall x [] = [[x]]
concatall x (y:ys) = [x:(y:ys)] ++ map (y:) (concatall x ys)

sign :: Int -> [Int] -> [[Int]]
sign n [] = [[]]
sign 0 xs = [xs]
sign n (x:xs) = map ((-1 * x):) (sign (n-1) xs) ++ negated 
    where
        negated = if length xs >= n then map (x:) (sign n xs) else []

result = concat $ map (\x -> concatMap (sign x) ps) [0..3]
    where
        ps = perms [1..3]

--stringify :: [[Int]] -> String
stringify xs = concat $ map (\x -> (listToString x) ++ "\n") xs
    where
        listToString = foldr (\fst rst -> (show fst) ++ " " ++ rst) ""
