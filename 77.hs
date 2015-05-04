import Data.List (findIndex)
import Data.Maybe (fromJust)

primesTo m = eratos [2 .. m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primes = primesTo 100
primesN = length primes

dp = [ rec n | n <- [0 .. primesN] ] where
    rec 0 = 1 : (replicate 100 0)
    rec n = result where
        result = zipWith (+) (dp!!(n-1)) ((replicate number 0) ++ result)
        number = primes !! (n - 1)

ways = [ (dp!!primesN)!!x | x <- [0 .. 100] ]

main = (print . fromJust) (findIndex (\x -> x > 5000) ways)
