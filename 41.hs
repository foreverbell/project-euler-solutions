
import Data.List (permutations)
import Common.Primes (testPrime)

-- 8 or 9-digit pandigital is divisable by 3 
main = print $ maximum candidate where
    candidate  = filter testPrime $ map read (permutations "1234567") :: [Int]
