
import Common.Numbers.Primes (testPrime)
import Common.Numbers.Numbers (inverse')

main = print $ sum $ [ inverse' (k - 1) p | p <- [a .. a + b - 1], testPrime p ] where
    k = 10^5
    a = 10^9
    b = 10^5
