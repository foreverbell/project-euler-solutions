
rec :: Int -> [Int]
rec 1 = repeat 1
rec n = result where 
    result = zipWith (+) last ((replicate n 0) ++ result)
    last = (rec (n - 1)) ++ [0]

main = print $ pred $ (rec 100) !! 100
