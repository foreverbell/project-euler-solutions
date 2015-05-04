
isqrt = floor . sqrt . fromIntegral
isSquared x = root * root == x where 
    root = isqrt x

accumulate (x:xs) = result where
    result = x : zipWith (+) xs result

count n = sum $ map find possible where
    possible = [ m | m <- [1 .. 2 * n], isSquared (m * m + n * n) ]
    find m = max (hi - lo + 1) 0 where  
        lo = (m `div` 2) + (m `mod` 2)
        hi = min (m - 1) n

main = print r where
    sols = [ count n | n <- [1 .. ] ]
    acc = zip [1 .. ] $ accumulate sols
    r = fst $ head $ dropWhile ((<= 1000000) . snd) acc

