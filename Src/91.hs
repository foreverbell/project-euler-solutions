
count n = 3*n*n + x where
    x = sum $ do
        x <- [1 .. n]
        y <- [1 .. n]
        let g = gcd x y
        return $ (min ((n - y) * g `div` x) (x * g `div` y)) + (min ((n - x) * g `div` y) (y * g `div` x))

main = print $ count 50
