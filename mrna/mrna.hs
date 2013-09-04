mrna :: Integral a => String -> a
mrna xs = (product $ map options xs) * options ' ' `mod` 1000000

options :: Integral a => Char -> a
options 'F' = 2
options 'L' = 6
options 'S' = 6
options 'Y' = 2
options 'C' = 2
options 'W' = 1
options 'P' = 4
options 'H' = 2
options 'Q' = 2
options 'R' = 6
options 'I' = 3
options 'M' = 1
options 'T' = 4
options 'N' = 2
options 'K' = 2
options 'V' = 4
options 'A' = 4
options 'D' = 2
options 'E' = 2
options 'G' = 4
options ' ' = 3