import Data.List
import Common.Primes (testPrime)

checkFromLeft :: Int -> Bool
checkFromLeft x = all testPrime $ map read $ init (tails (show x))

dfs :: Int -> [Int]
dfs x = if testPrime x
    then (x : (foldl (\s y -> s ++ (dfs (x * 10 + y))) [] [1,3,7,9]))
    else []

main = print $ (sum magic) - (2 + 3 + 5 + 7) where 
    candidate = (dfs 2) ++ (dfs 3) ++ (dfs 5) ++ (dfs 7)
    magic = filter checkFromLeft candidate
