
import Control.Arrow (first)
import System.Random (mkStdGen)
import Common.Numbers.Primes (factorize, toDivisors)

solve :: Int -> Int
solve n = n + 2 * k 
  where
    k = fst . last $ takeWhile (\(a, _) -> a <= n) p3
    p3 = (20, mkStdGen 23) : map next p3
    next (n, g) = (n + k, g0)
      where
        (ds, g0) = toDivisors `first` factorize g (2 * n - 1)
        k = minimum [((n `div` d) + 1) * d - n | d <- drop 1 ds]

main = print $ solve (10^15)
