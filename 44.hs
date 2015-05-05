import qualified Data.Set as S
import Data.List (sort)
import Common.List (nub')

{-- analysis:
 - It is equvilant to consider Q_n = n*(3n-1), denote the answer as Q_c/2,
 - then there exists a and b, with Q_b-Q_a=Q_c, by factorization, we get (b-a)(3b+3a-1)=Q_c,
 - then we can enumerate the divisor b-a, solve the equations to get a & b, and verify Q_a+Q_b by binary search.
 --}

isPentagonal :: Integer -> B2
              midValue = mid * (3 * mid - 1)

pFactor :: Integer -> [Integer]
pFactor x = nub' f where
    factor x = [ d | d <- [1 .. x], x `mod` d == 0 ]
    f1 = factor x
    f2 = factor (3*x-1)
    f  = [ a*b | a <- f1, b <- f2 ]

checkDiff index = any (\(a,b) -> isPentagonal (a*(3*a-1)+b*(3*b-1))) candidate where
    x = index * (3 * index - 1)
    f = pFactor index
    candidate = [ ((plus - minus) `div` 2, (plus + minus) `div` 2) | minus <- f, (x `div` minus + 1) `mod` 3 == 0, let plus = (x `div` minus + 1) `div` 3, plus > minus, even (plus - minus) ]

main = print $ n*(3*n-1) `div` 2 where 
    n = head $ dropWhile (not . checkDiff) [1 .. ]

