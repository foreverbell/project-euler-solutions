
import qualified Common.Numbers.Numbers as N
import qualified Common.Numbers.Primes as P
import qualified Common.Matrix.Int as M
import Data.List (foldl')

primes = take 100000 $ filter P.testPrime [10^14 .. ] :: [Int]

fibonacci :: Int -> Int -> Int
fibonacci m n = (base ^. n) M.! (1, 2) where
    base = M.fromList 2 2 [1, 1, 1, 0]
    (^.) = M.power add mul 
    add a b = (a + b) `rem` m
    mul a b = fromInteger $ ((toInteger a) * (toInteger b)) `rem` (toInteger m)

modulo = 1234567891011 :: Int

main = print $ foldl' (\a b -> (a + b) `rem` modulo) 0 $ map (fibonacci modulo) primes

