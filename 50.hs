
import Data.Array
import Data.Array.ST
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Function (on)
import Common.List (maximumBy')

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

solve :: [Int] -> (Int, Int)
solve [] = (0, 0)
solve primes@(p:ps) = maximumBy' cmp [best, (solve ps)] where
    cmp = compare `on` snd
    sum = takeWhile (<= 1000000) (scanl1 (+) primes)
    can = filter (isPrime . fst) (zip sum [1 .. ])
    best = maximumBy' cmp can

-- the low bound of answer is 21, so we only need to consider the primes below 1000000/20=50000.
main = print $ fst $ solve $ primesTo 50000

