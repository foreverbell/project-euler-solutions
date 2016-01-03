maxFactor x = helper x 2 where
    divide x p = until (\x -> x `mod` p /= 0) (`div` p) x
    helper x p | x < p * p      = x
               | x `mod` p == 0 = max p $ helper (divide x p) (p + 1)
               | otherwise      = helper x (p + 1)

main = print $ maxFactor 600851475143
