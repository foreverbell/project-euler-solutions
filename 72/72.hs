import Data.List (group, foldl')

primesTo m = eratos [2 .. m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primes = primesTo 1000

factoraize n
    | n == 1 = []
    | otherwise = helper primes n
    where
        helper [] n = [n]
        helper ps'@(p:ps) n
            | p * p > n = [n]
            | n `mod` p == 0 = p : (helper ps' $ n `div` p)
            | otherwise = helper ps n

totient n = foldl' (\r (x:_) -> (r `div` x) * (x - 1)) n (group fs)
    where
        fs = factoraize n

main = print $ (foldl' (\s x -> s + totient x) 0 [1 .. 1000000]) - 1
