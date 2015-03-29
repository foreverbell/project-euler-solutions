
import Data.Bits (xor)

solve :: Integer -> Integer
solve n = sum [ (cnt!!i) * (cnt!!j) * (cnt!!k) | i <- [0 .. 62], j <- [0 .. 62], k <- [0 .. 62], (i `xor` j `xor` k) /= 0] where
    cnt = [ (n `div` (2^i) + 1) `div` 2 | i <- [0 .. 62] ]

main = print $ (solve n) `mod` modulo where
    modulo = 1234567890
    n = 123456787654321

