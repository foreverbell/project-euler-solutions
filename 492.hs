
import Common.MapReduce (mapReduce)
import Common.Primes (testPrime)
import Common.Numbers (inverse, powMod)
import Data.Maybe (isJust)
import Data.List (find)

type Mat2 = (Int, Int, Int, Int)

multMat (a, b, c, d) (x, y, z, w) m = (a', b', c', d') where
    a' = (a * x + b * z) `mod` m
    b' = (a * y + b * w) `mod` m
    c' = (c * x + d * z) `mod` m
    d' = (c * y + d * w) `mod` m

addMat (a, b, c, d) (x, y, z, w) m = (a', b', c', d') where
    a' = (a + x) `mod` m
    b' = (b + y) `mod` m
    c' = (c + z) `mod` m
    d' = (d + w) `mod` m

powMat mat p m = helper mat p m (1, 0, 0, 1) where
    helper a 0 m ret = ret
    helper a p m ret = if odd p
        then helper a' p' m (multMat ret a m) 
        else helper a' p' m ret where
            a' = multMat a a m
            p' = p `div` 2

solve n p = ret where
    n' = fromIntegral $ powMod (2 :: Integer) (toInteger (n - 1)) (toInteger (p * p - 1))
    base = (0, p - 1, 1, 11)
    inv = (11, 1, p - 1, 0)
    mat = addMat (powMat base n' p) (powMat inv n' p) p
    ret = ((((first mat) - 5) `mod` p) * (inverse 6 p)) `mod` p
    first (a, _, _, _) = a

chunkSize = 10000

generatePrimes xs = mapReduce chunkSize (\x -> (x, testPrime x)) reduce xs where
    reduce =  (map fst) . (filter snd)

main = print $ mapReduce chunkSize (solve (10^15)) sum primes where
    primes = generatePrimes [a + 1, a + 3 .. a + b] :: [Int]
    a = 10^9
    b = 10^7
