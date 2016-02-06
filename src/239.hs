import Common.Numbers.Numbers (binomial, factorial)
import Text.Printf (printf)
import Data.Ratio

-- there are 25 primes below 100, answer = C(25,3) * #{permutation that 22 primes not in there natural position among remaining 97 numbers}
-- using inclusion-exclusion principle.

main = printf "%.12f\n" (ret :: Double)
  where
    ret = fromRational $ (binomial 25 3 * foolish22) % factorial 100
    foolish22 = sum $ [ factorial (97 - i) * (-1)^i * binomial 22 i | i <- [0 .. 22] ]
