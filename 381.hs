
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad

primesTo :: Int -> UArray Int Bool
primesTo m = runSTUArray $ do
    sieve <- newArray (2, m) True
    let root = (floor . sqrt . fromIntegral) m
    forM_ [2 .. root] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do
            forM_ [i^2, i^2+i .. m] $ \j -> do
                writeArray sieve j False
    return sieve

primes = drop 2 $ map fst $ filter (id . snd) $ assocs $ primesTo (10^8)

exgcd :: Int -> Int -> (Int, Int, Int)
exgcd a 0 = (a, 1, 0)
exgcd a b = (d, y, x - (a `div` b) * y) where
    (d, x, y) = exgcd b (a `mod` b)

modularInverse :: Int -> Int -> Int
modularInverse x m = if d /= 1
    then undefined
    else a `mod` m
    where (d, a, b) = exgcd x m

primeFactorial p = (-1 - a - ab - abc - abcd) `mod` p where
    a = p - 1 -- modularInverse (p - 1) p
    b = modularInverse (p - 2) p
    c = modularInverse (p - 3) p
    d = modularInverse (p - 4) p
    ab = (a * b) `mod` p
    abc = (ab * c) `mod` p
    abcd = (abc * d) `mod` p

solve = sum $ primes `seq` map primeFactorial primes

main = print solve
