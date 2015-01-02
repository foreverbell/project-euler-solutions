import Data.Array
import Data.List (maximumBy)
import Data.Function (on)

primesTo m = eratos [2 .. m] where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])

minus (x:xs) (y:ys) = case (compare x y) of 
    LT -> x : minus xs (y:ys)
    EQ -> minus xs ys 
    GT -> minus (x:xs) ys
minus xs _ = xs

isPrimeTable = listArray (1,bound) l where
    bound = 1000000
    helper n ps
        | n > bound = []
        | null ps || p /= n = False : (helper (n + 1) ps)
        | p == n = True : (helper (n + 1) (tail ps))
        where p = head ps
    l = helper 1 (primesTo bound)

isPrime = (!) isPrimeTable

solve :: [Int] -> (Int, Int)
solve [] = (0, 0)
solve primes@(p:ps) = maximumBy cmp [best, (solve ps)] where
    cmp = compare `on` snd
    sum = takeWhile (<= 1000000) (scanl1 (+) primes)
    can = filter (isPrime . fst) (zip sum [1 .. ])
    best = maximumBy cmp can

-- the low bound of answer is 21, so we only need to consider the primes below 1000000/20=50000.
main = print $ fst $ solve $ primesTo 50000

