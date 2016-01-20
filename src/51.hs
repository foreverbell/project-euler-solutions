
import Data.Char (chr, ord)
import Common.Utils (if')
import Common.Numbers.Primes (testPrime)

type Mask = [Int]

substitute :: Mask -> Int -> Int
substitute mask d = read (map (\x -> chr (x + ord '0')) (map (\x -> if' (x == -1) d x) mask)) 

goMask :: Int -> Int -> Int -> Mask -> Int
goMask top dep free mask = if dep == 0
    then if' (free == 3) (compute mask) maxBound
    else minimum $ map (\x -> goMask top (dep - 1) (free' x) (mask ++ [x])) can
    where
        free' x = free + (if' (x == -1) 1 0)
        can | top == dep = -1 : [1 .. 9]
            | dep == 1   = [1, 3, 7, 9]
            | otherwise  = [-1 .. 9]

compute :: Mask -> Int
compute mask = if' (total == 8) (head primes) maxBound where
    can = if' (head mask == -1) [1 .. 9] [0 .. 9]
    converted = map (substitute mask) can
    primes = filter testPrime converted
    total = length primes

main = print $ goMask 6 6 0 []
    
-- consider all valid masks, we can get some key observations:
-- 0. the answer is 6-digit long;
-- 1. the last digit must be 1,3,7,9;
-- 2. there should be exactly 3 free digits, otherwise, there will be at least 3 generated numbers divisable by 3.
