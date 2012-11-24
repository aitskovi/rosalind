-- Return the starting point of the locations of the substring in the
-- original string.

import Data.List

subs :: String -> String -> [Int]
subs xs "" = []
subs "" ys = []
subs (x:xs) ys = if isPrefixOf ys (x:xs) then 1:rest else rest
    where rest = map (+1) $ subs xs ys

run :: [String] -> [Int]
run (x1:x2:[]) = subs x1 x2
run _ = []

main = interact f
    where
        f = print . run . lines
        print = foldr (\fst rec -> show fst ++ " " ++ rec) "\n"

