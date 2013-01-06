-- This formula was derived from the numEdges = numNodes - 1
-- and the idea that numEdges/2 = sum of degree of each edge

inod :: Int -> Int
inod n = n - 2
