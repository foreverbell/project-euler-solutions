
import Common.Primes (primesTo)
import Data.List (group, foldl')

primes = primesTo 1000

factoraize 1 = []
factoraize n = helper primes n where
    helper [] n = [n]
    helper ps'@(p:ps) n
        | p * p > n = [n]
        | n `mod` p == 0 = p : (helper ps' $ n `div` p)
        | otherwise = helper ps n

totient n = foldl' (\r (x:_) -> (r `div` x) * (x - 1)) n (group fs) where
    fs = factoraize n

main = print $ (foldl' (\s x -> s + totient x) 0 [1 .. 1000000]) - 1
