-- http://www.haskell.org/haskellwiki/Prime_numbers

primes = eratos [2 .. ]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. ])

minus (x:xs) (y:ys) = case (compare x y) of 
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys 
    GT -> minus (x:xs) ys
minus xs _ = xs

main = do
    print (primes !! 10000)