-- Returns the number of subsets of a given set of size n % 1000000
sset :: Integral a => a -> a
sset n = 2^n `mod` 1000000
