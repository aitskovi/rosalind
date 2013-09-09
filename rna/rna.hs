rna :: [Char] -> [Char]
rna xs = replace 'T' 'U' xs

replace :: Char -> Char -> [Char] -> [Char]
replace a b xs = map (\x -> if x == a then b else x) xs
