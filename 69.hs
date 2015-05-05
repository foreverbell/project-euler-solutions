import Data.Array
import Common.Primes (primesTo)

find num (p:ps) = if num * p > 1000000 
    then num
    else find (num * p) ps

main = print $ find 1 (primesTo 1000)
