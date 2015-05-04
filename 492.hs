
import Control.Parallel.Strategies (parMap, rdeepseq, using, Strategy, NFData)
import Control.Parallel (pseq)
import Data.Maybe (isJust)
import Data.List (find)

powMod :: Int -> Int -> Int -> Int
powMod a p m = helper a p m 1 where
    helper a 0 m ret = ret
    helper a p m ret = if odd p
        then helper a' p' m (multiply ret a m)
        else helper a' p' m ret where
            a' = multiply a a m
            p' = p `div` 2
    multiply a b m = fromInteger ((a' * b') `mod` m') where
        a' = toInteger a
        b' = toInteger b
        m' = toInteger m

inverse x mod = powMod x (mod - 2) mod

millerRabin :: Int -> Int -> Bool
millerRabin n b = if (p == 1) || (p == n - 1) 
    then True
    else isJust $ find (== (n - 1)) (rec cnt p) where
        tail0 x cnt = if odd x 
            then (x, cnt)
            else (x `div` 2, cnt + 1)
        (m, cnt) = tail0 (n - 1) 0
        p = powMod b m n
        rec 0 p = []
        rec cnt p = p : (rec (cnt - 1) (p * p `mod` n))

testPrime :: Int -> Bool
testPrime 1 = False
testPrime 2 = True
testPrime 3 = True
testPrime 3215031751 = False
testPrime n = and $ map (millerRabin n) (takeWhile (< n) b) where
    b = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

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
    n' = powMod 2 (n - 1) (p * p - 1)
    base = (0, p - 1, 1, 11)
    inv = (11, 1, p - 1, 0)
    mat = addMat (powMat base n' p) (powMat inv n' p) p
    ret = ((((first mat) - 5) `mod` p) * (inverse 6 p)) `mod` p
    first (a, _, _, _) = a

mapReduce :: (NFData b, NFData c) => Int -> (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce n mapFunc reduceFunc xs = mapResult `pseq` reduceResult where
    mapResult = concat $ parMap rdeepseq (map mapFunc) (chunk n xs)
    reduceResult = reduceFunc mapResult `using` rdeepseq
    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = as : chunk n bs where (as, bs) = splitAt n xs

chunkSize = 10000

generatePrimes xs = mapReduce chunkSize (\x -> (x, testPrime x)) reduce xs where
    reduce =  (map fst) . (filter snd)

main = print $ primes `pseq` (mapReduce chunkSize (solve (10^15)) sum primes) where
    primes = generatePrimes [a + 1, a + 3 .. a + b] :: [Int]
    a = 10^9
    b = 10^7
