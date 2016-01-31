import Common.Numbers.Primes (testPrime, factorize, toDivisors)
import System.Random (mkStdGen)

-- real 3m47.134s
-- user 3m45.945s
-- sys  0m1.108s

-- http://mathworld.wolfram.com/BernoulliNumber.html
test :: Int -> Bool
test n = [2, 3, 5, 23, 29] == filter testPrime ds
  where 
    ds = map succ $ toDivisors $ fst $ factorize (mkStdGen 23) n

main = print (xs !! 99999)
  where
    xs = filter test [308, 308*2 .. ]
