
import Common.Primes (primes')

countPrimeFactors x = helper x primes' 0 where
    divide x p = until (\x -> x `rem` p /= 0) (`div` p) x
    helper x (p:ps) r | r >= 4         = 4  -- optimization
                      | x < p * p      = r + 1
                      | x `rem` p == 0 = helper (divide x p) ps (r + 1)
                      | otherwise      = helper x ps r

solve = helper pf 1 where
    pf = [ countPrimeFactors x | x <- [1 .. ] ]
    helper xs@(a:b:c:d:_) n = if a>=4 && b>=4 && c>=4 && d>=4
        then n
        else helper (tail xs) (n + 1)

main = print solve

