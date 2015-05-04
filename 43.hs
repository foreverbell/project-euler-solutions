
import Data.List (permutations, tails)

-- somehow a bit slow
check :: [Integer] -> Integer -> Bool
check [] x = True
check (y:ys) x
    | (x `mod` 1000) `mod` y == 0 = check ys (x `div` 10)
    | otherwise = False

main = print $ sum $ filter (check [17,13,11,7,5,3,2]) candidate
    where candidate = [ read x :: Integer | x <- (permutations "0123456789"), head x /= '0' ]
