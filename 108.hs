
primesTo m = eratos [2 .. m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primeTable = primesTo 100000

count n = (1 + (rec primeTable n 1)) `div` 2
    where
        fullDiv n p b
            | (n `mod` p == 0) = fullDiv (n `div` p) p (b + 1)
            | otherwise = (n, b)
        rec (p:ps) 1 res = res
        rec (p:ps) n res
            | p * p > n = res * 3
            | otherwise = rec ps a (res * (2 * b + 1))
            where (a, b) = fullDiv n p 0
                
main = print $ head $ filter (\n -> (count n) > 1000) [1 .. ]
