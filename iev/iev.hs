-- Parents:
--  * AA - AA -> 1
--  * AA - Aa -> 1
--  * AA - aa -> 1
--  * Aa - Aa -> 3/4
--  * Aa - aa -> 1/2
--  * aa - aa -> 0

dominantChild :: (Char, Char) -> (Char, Char) -> Rational
dominantChild (a, b) (c,d) = toRational (length (filter (any ('A' ==)) combinations)) / toRational (length combinations)
    where
        combinations = [[a,c],[a,d],[b,c],[b,d]]

-- dominantChild was used to calculate the number found above which were then used below.
-- Although it's not good to have magic numbers, this is a one-time program.

iev :: Float -> Float -> Float -> Float -> Float -> Float -> Float
iev a b c d e f = 2 * (a + b + c + 3/4 * d + 1/2 * e)
