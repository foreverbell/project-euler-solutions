import Data.List
import Common.Primes (testPrime)

rotate (x:xs) = xs ++ [x]

rotateNumber :: Int -> [Int]
rotateNumber x = map read strings where
    sx = show x
    strings = (sx : (unfoldr helper (rotate sx))) where 
        helper s = if s == sx
            then Nothing
            else Just (s, rotate s)

generate :: Int -> Int -> [Int]
generate 0 x = [x]
generate dep x = (x : (concatMap ((generate (dep - 1)) . ((+) (x * 10))) [1, 3, 7, 9]))

main = print $ length magic where
    magic = sort $ (2 : 5 : (filter helper candidate))
    helper x = all (\y -> y `elem` candidate) (rotateNumber x)
    candidate = filter testPrime (tail $ generate 6 0)

