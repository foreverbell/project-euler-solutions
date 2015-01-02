
import Data.List (permutations)

isPrime :: Int -> Bool
isPrime x
    | x <= 0    = False
    | x == 1    = False
    | otherwise = all (\y -> x `mod` y /= 0) [2 .. (floor $ sqrt $ fromIntegral x)]

-- 8 or 9-digit pandigital is divisable by 3 
main = print $ maximum candidate where
    candidate  = filter isPrime $ map read (permutations "1234567") :: [Int]
