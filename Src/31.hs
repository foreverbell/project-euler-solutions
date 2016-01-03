-- dynamic programming

dp :: [Int] -> Int -> [Int]
dp v p = ret where
    ret = [ (v !! i) + (f i) | i <- [0 .. (length v) - 1] ]
    f i = if i >= p 
        then ret !! (i - p)
        else 0

main = print $ v !! 200 where
    v = foldl dp dp0 [1, 2, 5, 10, 20, 50, 100, 200]
    dp0 = 1 : (take 200 $ repeat 0)
