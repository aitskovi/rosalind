append :: (Eq a) => a -> a -> [[a]] -> [[a]]
append c f [] = [[f]]
append c f (r:rs) = if f == c then []:(r:rs) else (f:r):rs

input :: [Char] -> [Rational]
input xs = map (\x -> toRational (read x::Double)) $ words xs

prob :: (Fractional b) => [Rational] -> [b]
prob xs = map (\x -> fromRational ((x * x + (1-x) * (1-x)) / 2)) xs

stringify :: (Show a) => [a] -> String
stringify xs = foldr (\fst rec -> (show fst) ++ " " ++ rec) "" xs
