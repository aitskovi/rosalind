import Utils.Bioinformatics

main = interactFasta $ print . gc
     where
         print x = fname x ++ "\n" ++ (show $ percent x) ++ "\n"

gc :: [Fasta] -> Fasta
gc xs = foldr (\x y -> if percent x > percent y then x else y) Empty xs

percent :: (Fractional a) => Fasta -> a
percent Empty = 0
percent (Fasta n d) =  gcCount / total * 100
    where
        gcCount = fromIntegral $ length $ filter (\x -> (x == 'C') || (x == 'G')) d
        total = fromIntegral $ length d

-- Generate a string from a Fasta DataType
stringify :: Fasta -> String
stringify (Fasta name value) = name ++ "\n" ++ value

fname :: Fasta -> String
fname Empty = "Empty"
fname (Fasta n d) = n

