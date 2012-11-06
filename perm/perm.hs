perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (concatall x) (perms xs))

concatall :: a -> [a] -> [[a]]
concatall x [] = [[x]]
concatall x (y:ys) = [x:(y:ys)] ++ map (y:) (concatall x ys)

-- Unused for Question --

subset :: Int -> [a] -> [[a]]
subset n [] = []
subset 1 xs = map (\x -> [x]) xs
subset n (x:xs) = (map (x:) $ subset (n - 1) xs) ++ rest
    where
        rest = if length xs >= n then subset n xs else []

subperms :: Int -> [a] -> [[a]]
subperms n xs = concat $ map perms $ subset n xs
