
import Common.Primes (primesTo)
import Data.List (sort)
import Control.Monad (filterM)

product1 [] = 1
product1 xs = product xs

powerset xs = filterM (const [True, False]) xs

primes :: [Integer]
primes = map toInteger $ primesTo 190

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
