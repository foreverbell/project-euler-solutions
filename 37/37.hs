import Data.List

isPrime :: Integer -> Bool
isPrime x
    | x <= 0    = False
    | x == 1    = False
    | otherwise = all (\y -> x `mod` y /= 0) [2 .. (floor $ sqrt $ fromIntegral x)]

checkFromLeft :: Integer -> Bool
checkFromLeft x = all isPrime $ map read $ init (tails (show x))

dfs :: Integer -> [Integer]
dfs x
    | isPrime x = (x : (foldl (\s y -> s ++ (dfs (x * 10 + y))) [] [1,3,7,9]))
    | otherwise = []

main = do
    print $ (sum magic) - (2 + 3 + 5 + 7)
    where candidate = (dfs 2) ++ (dfs 3) ++ (dfs 5) ++ (dfs 7)
          magic = filter checkFromLeft candidate
