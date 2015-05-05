
import Common.Primes (primesTo)
import Common.Numbers (inverse')
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad

primes = drop 2 $ primesTo (10^8)

primeFactorial p = (-1 - a - ab - abc - abcd) `mod` p where
    a = p - 1 -- inverse' (p - 1) p
    b = inverse' (p - 2) p
    c = inverse' (p - 3) p
    d = inverse' (p - 4) p
    ab = (a * b) `mod` p
    abc = (ab * c) `mod` p
    abcd = (abc * d) `mod` p

solve = sum $ map primeFactorial primes

main = print solve
