
import Data.Array
import Data.Array.ST
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Char (chr, ord)
import Data.List (findIndex)

isPrimeTo m = runSTArray $ do
    sieve <- newArray (2, m) True :: ST s (STArray s Int Bool)
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

primesTo m = map fst $ filter (id . snd) $ assocs $ isPrimeTo m

isPrimeTable = isPrimeTo 1000000
isPrime 0 = False
isPrime 1 = False
isPrime x = isPrimeTable ! x

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
