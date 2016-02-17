-- check overview for a faster solution.

import Common.Numbers.Primes (testPrime)

main = print $ length $ filter (testPrime . t) [1 .. 50000000]
  where t n = 2*n*n - 1 :: Int
