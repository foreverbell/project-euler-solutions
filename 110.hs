import Common.Primes (primes)

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

main = print $ dfs primeTable 20 1 1 upperBound where
    primeTable = take 14 $ primes -- 3**14 > 3999999
    upperBound = 10^100

