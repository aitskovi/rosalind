-- Return the starting point of the locations of the substring in the
-- original string.

subs :: String -> String -> [Int]
subs xs "" = []
subs "" ys = []
subs xs ys = reverse $ subsHelper indexed ys []
    where
        indexed = zip [1..(length xs)] xs

subsHelper :: [(Int, Char)] -> String -> [(Int, String)] -> [Int]
subsHelper [] ys zs = map fst $ filter (\(index, remainder) -> (length remainder) == 0) zs
subsHelper ((x1, x2):xs) (y:ys) zs = subsHelper xs (y:ys) matches
    where
        updated = updatePatterns x2 zs
        matches = if y == x2 then (x1, ys):updated else updated

updatePatterns :: Char -> [(Int, String)] -> [(Int, String)]
updatePatterns c [] = []
updatePatterns c ((index, []):ps) = (index, []):(updatePatterns c ps)
updatePatterns c ((index, (r:rs)):ps) = if r == c then (index, rs):rest else rest
    where
        rest = updatePatterns c ps

run :: [String] -> [Int]
run (x1:x2:[]) = subs x1 x2
run _ = []

main = interact f
    where
        f = print . run . lines
        print = foldr (\fst rec -> show fst ++ " " ++ rec) "\n"

