-- Calculates the probability that an offspring will inherit
-- a dominant gene given random parents in the population:
--  k -> Num Homozygous
--  m -> Num Heterozygous
--  n -> Num Homozygous Recessive

iprb :: Float -> Float -> Float -> Float
iprb k m n = homozygousA + dominantA + homozygousB + dominantB + dominantB'
    where
        total = k + m + n
        homozygousA = k/total
        dominantA = m/total * 1/2
        recessiveA = dominantA
        heterozygousA = n/total
        homozygousB = (1 - homozygousA - dominantA) * k/(total - 1)
        dominantB = heterozygousA * m/(total - 1) * 1/2
        dominantB' = recessiveA * (m-1)/(total - 1) * 1/2
