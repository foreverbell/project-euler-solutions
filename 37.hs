import Data.List (all)
import Common.Numbers.Primes (testPrime)

main = print $ ret - (2 + 3 + 5 + 7) where 
    ret = sum $ filter check $ concat $ map get [2, 3, 5, 7]
    check x = (testPrime x) && (all testPrime (takeWhile (< x) (map (\d -> x `mod` d) ds))) where
        ds = 10 : map (* 10) ds
    get x = if testPrime x
        then x : concat (map (\y -> get (x * 10 + y)) [1, 3, 7, 9])
        else []

