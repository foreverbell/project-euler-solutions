import Data.List (sort)

fibonacciTail :: [Int]
fibonacciTail = 1 : 1 : zipWith (\x y -> (x + y) `mod` 1000000000) fibonacciTail (tail fibonacciTail)

fibonacciHead :: [Int]
fibonacciHead = [ gen n | n <- [1 .. ] ]
    where
        logPhi = log ((1 + sqrt 5) / 2) / (log 10) :: Double
        logSqrt5 = (log (sqrt 5)) / (log 10) :: Double
        gen n = (round $ 10 ** (t - (fromIntegral (floor t)) + 10)) `div` 100
            where t = logPhi * n - logSqrt5 :: Double

main = print $ solve fibonacciHead fibonacciTail 1
    where
        pandigital xs = (sort xs) == "123456789"
        check = pandigital . show
        solve (x:xs) (y:ys) index 
            | (check x) && (check y) = index
            | otherwise = solve xs ys (index + 1)
