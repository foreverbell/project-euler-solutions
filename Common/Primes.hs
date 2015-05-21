module Common.Primes (
    primes,
    primes',
    primesTo, 
    testPrime
) where

import Control.Monad (forM_, when)
import Data.Array.Unboxed
import Data.Array.ST
import Data.Maybe (isJust)
import Data.List (find)
import Data.Bits (shiftR, (.&.))
import Common.Numbers (powMod)
import Common.Util (isqrt)

primes :: [Int]
primes = 2 : eratos [3, 5 .. ] where
    eratos (p:xs) = p : eratos (xs `minus` [p^2, p^2+p .. ])
    minus xs'@(x:xs) ys'@(y:ys) = case (compare x y) of 
        LT -> x : minus xs ys'
        EQ -> minus xs ys 
        GT -> minus xs' ys
    minus xs _ = xs

primes' :: [Int]
primes' = 2 : filter testPrime [3, 5 .. ]

primesTo :: Int -> [Int]
primesTo m = map fst $ filter (id . snd) $ assocs $ runSTUArray $ do
    sieve <- newArray (2, m) True
    let root = isqrt m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

primes10k = primesTo 10000

-- prime test 
millerRabinTest :: Int -> Int -> Bool
millerRabinTest n b = if (p == 1) || (p == n - 1) || (n == b)
    then True
    else (n `rem` b /= 0) && (rec cnt p) where
        tail0 x cnt = if (x .&. 1) == 1 
            then (x, cnt)
            else tail0 (x `shiftR` 1) (cnt + 1)
        (m, cnt) = tail0 (n - 1) 0
        p = if (n < 2^31)
            then powMod b m n
            else fromIntegral $ powMod (toInteger b) (toInteger m) (toInteger n)
        rec 0 p = False
        rec cnt p = if (p2 == n - 1)
            then True
            else rec (cnt - 1) p2 where
                p2 = if (p < 2^31)
                    then p^2 `rem` n
                    else fromIntegral $ (toInteger p)^2 `rem` (toInteger n)

naiveTest :: Int -> Bool
naiveTest n = all (\d -> n `rem` d /= 0) $ takeWhile (<= root) primes10k where
    root = isqrt n

testPrime :: Int -> Bool
testPrime 1 = False
testPrime 2 = True
testPrime 3 = True
testPrime 3215031751 = False
testPrime n = if (n <= 0)
    then False
    else if n >= 100000000
        then and $ map (millerRabinTest n) (takeWhile (< n) b) 
        else naiveTest n 
    where 
        b = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]
