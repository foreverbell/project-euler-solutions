import Common.Numbers.Primes (countPrime', primesTo)
import Common.Utils (isqrt)
import qualified Data.Map as M
import Data.Maybe (fromJust)

n = 10^8 :: Int

primes = primesTo $ isqrt n

count = M.fromList $ countPrime' n

solve m | from > to = 0
        | otherwise = fromJust (M.lookup to count) - fromJust (M.lookup (from - 1) count)
  where
    from = m
    to = n `div` m

main = print $ sum $ map solve primes
