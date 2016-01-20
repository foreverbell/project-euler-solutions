import Common.Numbers.Primes (primesTo)
import Common.Numbers.Numbers (powMod)

solve n modulo = productMod modulo $ map (\x -> ((powMod x (countExponent n x) modulo)^2 + 1) `mod` modulo) primes where
    productMod m xs = foldl helper 1 xs where
        helper accum x = (accum * x) `mod` m
    primes = primesTo n
    countExponent n p = sum $ takeWhile (/= 0) rs where
        rs = (n `div` p) : map (\x -> x `div` p) rs

main = print $ solve (10^8) (10^9+9)
