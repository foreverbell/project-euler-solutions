import Common.Numbers.Primes (primesTo)
import Common.List (maximum')

n = 10000000 :: Int

primes = primesTo n :: [Int]

solve :: Int -> Int -> Int
solve p q = maximum' xs
  where
    ps = p : map (\p0 -> p0 * p) ps
    qs = q : map (\q0 -> q0 * q) qs
    xs = [ p0 * q0 | p0 <- takeWhile (<= n) ps, q0 <- takeWhile (<= n `div` p0) qs ]

main = print $ sum [ solve p q | p <- primes, q <- takeWhile (\q -> q <= n `div` p && q < p) primes ]
