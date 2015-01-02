import Data.Array
import Data.Char (chr, ord)
import Data.List (findIndex)

primesTo m = eratos [2 .. m] where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])

minus (x:xs) (y:ys) = case (compare x y) of 
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys 
    GT -> minus (x:xs) ys
minus xs _ = xs

isPrimeTable = listArray (1,bound) l where
    bound = 1000000
    helper n ps
        | n > bound = []
        | null ps || p /= n = False : (helper (n + 1) ps)
        | p == n = True : (helper (n + 1) (tail ps))
        where p = head ps
    l = helper 1 (primesTo bound)

isPrime = (!) isPrimeTable

type Mask = [Int]

substitute :: Mask -> Int -> Int
substitute mask d = read (map (\x -> chr (x + ord '0')) (map (\x -> if x == -1 then d else x) mask)) :: Int

goMask :: Int -> Int -> Int -> Mask -> Int
goMask top dep free mask
    | dep == 0 = if (free == 3) then compute mask else (maxBound :: Int)
    | otherwise = minimum $ map (\x -> goMask top (dep - 1) (free' x) (mask ++ [x])) can
    where
        free' x = free + (if x == -1 then 1 else 0)
        can
            | top == dep = -1 : [1 .. 9]
            | dep == 1   = [1, 3, 7, 9]
            | otherwise  = [-1 .. 9]

compute :: Mask -> Int
compute mask
    | total == 8 = head primes
    | otherwise = maxBound :: Int
    where
        can
            | head mask == -1 = [1 .. 9]
            | otherwise       = [0 .. 9]
        converted = map (substitute mask) can
        primes = filter isPrime converted
        total = length primes

main = print $ goMask 6 6 0 []
    
-- consider all valid masks, we can get some key observations:
-- 0. the answer is 6-digit long;
-- 1. the last digit must be 1,3,7,9;
-- 2. there should be exactly 3 free digits, otherwise, there will be at least 3 generated numbers divisable by 3.
