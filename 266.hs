
import Data.List (sort)
import Control.Monad (filterM)

product1 [] = 1
product1 xs = product xs

powerset xs = filterM (const [True, False]) xs

primesTo m = eratos [2 .. m] where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

primes :: [Integer]
primes = primesTo 190

n = product primes

scanZip [] _ = []
scanZip _ [] = []
scanZip xs'@(x:xs) ys'@(y:ys) = case compare ((x*y)^2) n of
    LT -> (x*y) : scanZip xs ys'
    _ -> scanZip xs' ys

solve = ret `mod` (10^16) where
    first21 = take 21 primes
    last21 = drop 21 primes
    xs = sort $ map product1 $ powerset first21
    ys = reverse $ sort $ map product1 $ powerset last21
    ret = maximum $ scanZip xs ys
    
main = print solve
