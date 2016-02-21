import Common.Numbers.Primes (primes')
import Common.Numbers.Numbers (powMod)

check :: Int -> Int -> Bool
check _ 2 = False
check _ 5 = False
check n p = powMod 10 (gcd n (p-1)) p == 1 && ((sum0 xs) * q `rem` p + sum0 (take r xs)) `rem` p == 0
  where
    get xs = 1 : takeWhile (/= 1) (tail xs)
    xs = get $ iterate (\x -> x * 10 `rem` p) 1
    (q, r) = n `quotRem` length xs
    sum0 = foldr (\x y -> (x+y) `rem` p) 0
    
main = print $ sum $ take 40 $ filter (check $ 10^9) primes'
