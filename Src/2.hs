main = print $ sum $ filter even $ takeWhile (<= 4000000) fib where
    fib = 1 : 2 : zipWith (+) fib (tail fib)
