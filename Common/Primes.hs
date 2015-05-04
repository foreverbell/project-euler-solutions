module Common.Primes (
    primesTo, 
    testPrime
) where

import Control.Monad (forM_, when)
import Data.Array.Unboxed
import Data.Array.ST
import Data.Maybe (isJust)
import Data.List (find)
import Common.Numbers (powMod)

primesTo :: Int -> [Int]
primesTo m = map fst $ filter (id . snd) $ assocs $ runSTUArray $ do
    sieve <- newArray (2, m) True
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

-- prime test 
millerRabin :: Int -> Int -> Bool
millerRabin n b = if (p == 1) || (p == n - 1) 
    then True
    else isJust $ find (== (n - 1)) (rec cnt p) where
        tail0 x cnt = if odd x 
            then (x, cnt)
            else (x `div` 2, cnt + 1)
        (m, cnt) = tail0 (n - 1) 0
        p = fromIntegral $ powMod (toInteger b) (toInteger m) (toInteger n)
        rec 0 p = []
        rec cnt p = p : (rec (cnt - 1) (p * p `mod` n))

testPrime :: Int -> Bool
testPrime 1 = False
testPrime 2 = True
testPrime 3 = True
testPrime 3215031751 = False
testPrime n = and $ map (millerRabin n) (takeWhile (< n) b) where
    b = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
