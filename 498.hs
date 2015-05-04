
import Common.Numbers (inverse')
import Data.List (foldl')

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
        b = product' [inverse' i p | i <- [1 .. m]] p
        c = product' [inverse' i p | i <- [1 .. n - m]] p

modulo = 999999937
n = 10^13
m = 10^12
d = 10^4

main = print $ (binomial n d modulo) * (binomial (n - d - 1) (m - d - 1) modulo) `mod` modulo
