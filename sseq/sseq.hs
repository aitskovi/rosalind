import Utils.Bioinformatics

main = interactFasta run
    where
        run xs = unwords $ map show $ sseq (xs !! 1) (xs !! 0)

sseq :: Fasta -> Fasta -> [Int]
sseq (Fasta a b) (Fasta c d) = subsequence b d

subsequence :: Eq a => [a] -> [a] -> [Int]
subsequence xs ys = subsequenceIndex xs ys 1

subsequenceIndex :: Eq a => [a] -> [a] -> Int -> [Int]
subsequenceIndex [] ys n = []
subsequenceIndex xs [] n = []
subsequenceIndex (x:xs) (y:ys) n = if x == y then n : subsequenceIndex xs ys (n+1)
                                             else subsequenceIndex (x:xs) ys (n+1)
