import Data.List (sort)
import Data.Function (on)
import Common.Primes (primesTo)
import Common.List (minimumBy')

main = print $ (fst answer) * (snd answer) where
    primes = primesTo 5000
    candidate = [ (p,q) | p <- primes, q <- primes, p*q <= 10000000, check p q ]
    value :: (Int, Int) -> Double
    value (p,q) = p' * q' / (p' - 1) / (q' - 1) where 
        p' = fromIntegral p
        q' = fromIntegral q
    check p q = f (p * q) == f (phi p q) where
        f = sort . show
        phi p q = (p - 1) * (q - 1)
    answer = minimumBy' (compare `on` value) candidate

-- nice try! 
-- although I can't ensure this method produces the best answer :(
