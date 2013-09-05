pper :: Integral a => a -> a -> a
pper n k = product [(n-k+1)..n] `mod` 1000000
