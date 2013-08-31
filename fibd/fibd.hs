import Data.Array

-- Fibionacci sequence where bunnies die m months after they are born.
-- F_0 is also defined for ease of use as we need it for defining how
-- many bunnies were born in first step.
fibd n m = fibds ! n
    where
        fibds = array (0, n) (((0,1) : zip [1..m] (take m fibs)) ++
                                [(i, fibds!(i-1) + fibds!(i-2) - fibds!(i-m-1)) | i <- [m+1..n]])

-- Normal fibionacci sequence
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
