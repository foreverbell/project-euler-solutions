import Data.List (findIndex)
import Data.Maybe (fromJust)
import Common.Primes (primesTo)

primes = primesTo 100
primesN = length primes

dp = [ rec n | n <- [0 .. primesN] ] where
    rec 0 = 1 : (replicate 100 0)
    rec n = result where
        result = zipWith (+) (dp!!(n-1)) ((replicate number 0) ++ result)
        number = primes !! (n - 1)

ways = [ (dp!!primesN)!!x | x <- [0 .. 100] ]

main = (print . fromJust) (findIndex (\x -> x > 5000) ways)
