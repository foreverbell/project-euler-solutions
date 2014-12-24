import Data.List (sort, minimumBy)
import Data.Function (on)

primesTo m = eratos [2 .. m] where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs


main = print $ (fst answer) * (snd answer)
    where
        primes = primesTo 5000
        candidate = [ (p,q) | p <- primes, q <- primes, p*q <= 10000000, check p q ]
        value :: (Int, Int) -> Double
        value (p,q) = p' * q' / (p' - 1) / (q' - 1)
            where p' = fromIntegral p
                  q' = fromIntegral q
        check p q = f (p * q) == f (phi p q) 
            where
                f = sort . show
                phi p q = (p - 1) * (q - 1)
        answer = minimumBy (compare `on` value) candidate

-- nice try! 
-- although I can't ensure this method produces the best answer :(
