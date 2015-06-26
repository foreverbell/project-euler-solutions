
import Common.Numbers.Primes (primesTo, countPrime')
import qualified Data.Map as M

iroot x p = floor $ (fromIntegral x) ** (1/p)

n = 10^12 :: Int
piMap = M.fromList $ countPrime' n
count = (M.!) piMap

main = print $ a + b + c + d where
    primes4 = zip [1 .. ] $ primesTo $ iroot n 4 
    primes2 = primesTo $ iroot n 2
    a = count $ iroot n 7
    b = sum $ map f primes4 where
        f (i, p) = count (iroot (n `div` p) 3) - i
    c = sum $ map f primes4 where
        f (i, p) = count (n `div` (p^3)) - i
    d = rec 1 1 1 primes2 where
        rec _ _ _ [] = 0
        rec 1 x i (p:ps) = if (p*p*p > n)
            then 0
            else (rec 2 (x*p) (i+1) ps) + (rec 1 x (i+1) ps)
        rec 2 x i (p:ps) = if (x*p*p > n)
            then 0
            else (count (n `div` (x*p))) - i + (rec 2 x (i+1) ps) where
