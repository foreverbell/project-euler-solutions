main = do
    print (foldr acc 0 fib)
    where fib = 1 : 2 : zipWith (+) fib (tail fib)
          acc x sum
            | x > 4000000 = 0
            | even x = sum + x
            | otherwise = sum
