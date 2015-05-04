
module Common.Primes (
    primesTo
) where

import Control.Monad (forM_, when)
import Data.Array.Unboxed
import Data.Array.ST

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
