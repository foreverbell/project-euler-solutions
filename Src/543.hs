import           Common.Numbers.InfiniteSequence (fibnoacci)
import           Common.Numbers.Primes (isPrimeTableTo)
import qualified Data.Vector.Unboxed as V

-- Goldbach's conjecture

fib :: [Int]
fib = reverse $ take 45 fibnoacci

isPrimeTable = isPrimeTableTo $ head fib

contrib n = length $ takeWhile (>= n) fib

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

solve :: Int
solve = sum $ zipWith (\i w -> contrib i * w) [0 .. ] ways
  where
    to = head fib
    ways = [0, 0, 1, 1, 1, 2, 2, 3] ++ [ f i | i <- [8 .. to] ]
    f i | even i = i `quot` 2 - 1
        | otherwise = fromBool (isPrimeTable V.! i) + fromBool (isPrimeTable V.! (i - 2)) + i `quot` 2 - 2

main = print solve
