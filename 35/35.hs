import Data.List

rotate (x:xs) = xs ++ [x]

rotateNumber :: Int -> [Int]
rotateNumber x = map read strings where
    sx = show x
    strings = (sx : (unfoldr helper (rotate sx))) where 
        helper s
            | s == sx   = Nothing
            | otherwise = Just (s, rotate s)

isPrime :: Int -> Bool
isPrime x
    | x <= 0    = False
    | x == 1    = False
    | otherwise = all (\y -> x `mod` y /= 0) [2 .. (floor $ sqrt $ fromIntegral x)]

generate :: Int -> Int -> [Int]
generate 0 x = [x]
generate dep x = (x : (concatMap ((generate (dep - 1)) . ((+) (x * 10))) [1, 3, 7, 9]))

main = print $ length magic where
    magic = sort $ (2 : 5 : (filter helper candidate))
    helper x = all (\y -> y `elem` candidate) (rotateNumber x)
    candidate = filter isPrime (tail $ generate 6 0)

