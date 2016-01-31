import Common.Numbers.Primes (primes')
import Common.Numbers.Numbers (powMod)
import Data.Maybe (fromJust)
import Data.List (find)

r p n = (powMod (p - 1) n m + powMod (p + 1) n m) `mod` m
  where m = p * p

main = print $ snd $ fromJust $ find (\(p, n) -> r (toInteger p) n > 10^10) $ zip primes' ([1 .. ] :: [Int])
