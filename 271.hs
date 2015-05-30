
import Common.Numbers.Numbers (crt2, inverse')
import Common.Numbers.Primes (primesTo)
import Control.Monad (foldM)

primes :: [Integer]
primes = map toInteger $ primesTo 43 -- product primes == 13082761331670030

get p = [ (p, x) | x <- [1 .. p - 1], (x ^ 3) `mod` p == 1 ]

solve :: [(Integer, Integer)]
solve = foldM (\(ps, a) xs -> map (\(p, x) -> (ps * p, crt2 (ps, a) (p, x))) xs) (1, 1) $ map get primes

main = print $ pred $ sum $ map snd solve
