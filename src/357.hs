
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad
import Common.Utils (isqrt)

primesTo :: Int -> UArray Int Bool
primesTo m = runSTUArray $ do
    sieve <- newArray (2, m) True
    let root = isqrt m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

primes = primesTo (10^8)
primeList = map fst $ filter (id . snd) $ assocs primes

solve = solveIter primeList 0 where
    solveIter [] sum = sum
    solveIter (p:ps) sum = solveIter ps sum' where
        sum' = sum + delta
        k = p - 1
        root = isqrt k
        wanted = and $ do
            d <- [1 .. root]
            guard ((k `mod` d) == 0)
            return $ primes ! (d + k `div` d)
        delta = if wanted then k else 0

main = print solve
