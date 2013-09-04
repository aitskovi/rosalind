lia :: (Integral a, Fractional b) => a -> a -> b
lia k n = fromRational $ 1 - sum bad
    where
        size = 2^k
        bad = map (binomial (1/4) size) [0..n-1]

-- Computes a binomial coefficient with probabilty p where we
-- want k of n things to be "True"
binomial :: Integral a => Rational -> a -> a -> Rational
binomial p n k = (toRational (choose n k)) * p^k * (1 - p)^(n-k)

-- Returns the number of ways to choose k elements from n elements
--choose :: Integral a => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- Generates the probabilities for the "nth" generation given
-- an original generation.
--
-- This converges to (0.25, 0.5, 0.25) when started with (0, 1, 0)
generation :: (Fractional a) => Int -> (a,a,a) -> (a,a,a)
generation 0 population = population
generation n population = generation (n - 1) $ mate population 

-- Take a probability generation of AA, Aa, aa and
-- mate them with an Aa. Giving the probability of an offspring.  
mate :: (Fractional a) => (a, a, a) -> (a, a, a)
mate (x, y, z)  = (1/2 * x + 1/4 * y, 1/2 * x + 1/2 * y + 1/2 * z , 1/4 * y + 1/2 * z)
