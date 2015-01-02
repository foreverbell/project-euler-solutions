import Data.Foldable (maximumBy)
import Data.Function (on)

isPrime :: Int -> Bool
isPrime x
    | x <= 0    = False
    | x == 1    = False
    | otherwise = all (\y -> x `mod` y /= 0) [2 .. (floor $ sqrt $ fromIntegral x)]

produceLength (a, b) = length $ takeWhile (isPrime . (\n -> n^2+a*n+b)) [0 .. ]

main = print $ uncurry (*) result where
    result = maximumBy (compare `on` produceLength) $ [ (a,b) | a <- [-999 .. 999], b <- [-999 .. 999] ]
