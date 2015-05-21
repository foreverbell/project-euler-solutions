
import Common.Primes (primesTo)

count n p = sum $ takeWhile (/= 0) ps where
    ps = (n `div` p) : map (\x -> x `div` p) ps

main = print $ sum $ [ p * ((count n p) - (count m p) - (count (n - m) p)) | p <- primes ] where 
    primes = primesTo n
    n = 20000000
    m = 15000000
