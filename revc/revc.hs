revc :: [Char] -> [Char]
revc xs = map complement (reverse xs)

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'
