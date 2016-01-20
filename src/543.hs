import           Common.Numbers.InfiniteSequence (fibnoacci)
import           Common.Numbers.Primes (primesTo')
import qualified Data.Vector.Unboxed as V
import           Data.List (foldl')

-- Goldbach's conjecture

fib :: [Int]
fib = reverse $ take 45 fibnoacci

isPrimeTable = fst $ primesTo' (head fib)

contrib n = length $ takeWhile (>= n) fib

fromBool :: Bool -> Int
fromBool True = 1
fromBool False = 0

solve :: Int
solve = foldl' (\r i -> r + contrib i * f i) 0 [1 .. to] 
  where
    to = head fib
    f i | i <= 7 = [0, 0, 1, 1, 1, 2, 2, 3] !! i
        | even i = i `quot` 2 - 1
        | otherwise = fromBool (isPrimeTable V.! i) + fromBool (isPrimeTable V.! (i - 2)) + i `quot` 2 - 2

main = print solve
