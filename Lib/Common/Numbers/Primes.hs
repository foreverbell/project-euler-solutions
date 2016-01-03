module Common.Numbers.Primes (
    primes,
    primes',
    primesTo, 
    testPrime,
    countPrimeApprox,
    countPrime,
    countPrime'
) where

import Control.Monad (forM_, when)
import Data.Array.Unboxed
import Data.Array.ST
import Data.Bits (shiftR, (.&.))
import Common.Numbers.Numbers (powMod)
import Common.Util (isqrt, if')

primes :: [Int]
primes = 2 : eratos [3, 5 .. ] where
    eratos [] = []
    eratos (p:xs) = p : eratos (xs `minus` [p^2, p^2+p .. ])
    minus xs'@(x:xs) ys'@(y:ys) = case compare x y of 
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
        rec 0 _ = False
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

-- never underestimated for n <= 10^12
countPrimeApprox :: Int -> Int
countPrimeApprox = truncate . appi . fromIntegral where
    appi x = y - y / 300000 + 7 * l' where
        y = x * l * (1 + l * (1 + l * h))
        w = log x
        l = 1 / w
        l' = log w
        h | x < 10000000  = 2.5625
          | x < 50000000  = 2.5
          | x < 120000000 = 617 / 256
          | x > 10**12    = undefined
          | otherwise     = 2.0625 + l * (3 + l' * l * (13.25 + l' * l * 57.75))

countPrime :: Int -> Int 
countPrime = snd . head . countPrime'

countPrime' :: Int -> [(Int, Int)]
countPrime' n = zip (map snd v) $ elems dynamic where
    root = isqrt n
    ps = zip [0 .. ] $ primesTo (root + 1)
    last = n `div` (root + 1)
    v = zip [0 .. ] $ (map (\i -> n `div` i) [1 .. root + 1]) ++ [last - 1, last - 2 .. 0]
    dynamic = runSTUArray $ do
        dp <- newListArray (0, (length v) - 1) $ map ((max 0) . pred . snd) v
        forM_ ps $ \(i, p) -> do
            let p2 = p * p
            forM_ (takeWhile (\(_, k) -> k >= p2) v) $ \(j, k) -> do
                let k' = k `div` p
                let j' = pred $ if' (k' < last) (root + 1 + last - k') (n `div` k')
                v1 <- readArray dp j
                v2 <- readArray dp j'
                writeArray dp j (v1 - v2 + i)
        return dp
