
import Data.List (foldl')

exgcd :: Int -> Int -> (Int, Int, Int)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `div` b) * y) where
    (d, x, y) = exgcd b (a `mod` b)

modularInverse :: Int -> Int -> Int
modularInverse x m = if d /= 1
    then undefined
    else a `mod` m
    where (d, a, b) = exgcd x m

product' xs p = foldl' helper 1 xs where
    helper accum x = (accum * x) `mod` p

binomial n m p = if n < m 
    then 0
    else if (n > p)
        then (binomial n' m' p) * (binomial n'' m'' p) `mod` p 
        else ((a * b) `mod` p) * c `mod` p
    where
        n' = n `div` p
        m' = m `div` p
        n'' = n `mod` p
        m'' = m `mod` p
        a = product' [1 .. n] p
        b = product' [modularInverse i p | i <- [1 .. m]] p
        c = product' [modularInverse i p | i <- [1 .. n - m]] p

modulo = 999999937
n = 10^13
m = 10^12
d = 10^4

main = print $ (binomial n d modulo) * (binomial (n - d - 1) (m - d - 1) modulo) `mod` modulo
