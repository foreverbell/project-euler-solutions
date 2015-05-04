
primesTo m = eratos [2 .. m]  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primeTable = take 14 $ primesTo 100 -- 3**14 > 4000000
upperBound = (toInteger . round) (10 ** 100)

dfs :: [Int] -> Int -> Int -> Integer -> Integer -> Integer
dfs (p:ps) t total num best 
    | num > best = best
    | otherwise = minimum result
    where
        result = [ dfs ps i (total * (2 * i + 1)) j (minimum (best : (take i result))) | (i, j) <- (zip [0 .. t] pow) ]
        pow = num : map (* (toInteger p)) pow
dfs [] t total num best = case compare total 8000000 of
    GT -> min best num
    _  -> best

main = print $ dfs primeTable 20 1 1 upperBound

