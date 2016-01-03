main = print $ snd . head $ rest where
    fib' = 1 : 1 : zipWith (+) fib' (tail fib')
    fib  = zip fib' [1 .. ]
    rest = dropWhile (\(f, index) -> (length . show $ f) < 1000) fib
