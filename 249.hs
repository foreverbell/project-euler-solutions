
import Common.Primes (primesTo, testPrime)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.List (foldl')

dynamic :: Int -> [Int] -> V.Vector Int -> V.Vector Int
dynamic _ [] dp = dp
dynamic modulo (x:xs) dp = dynamic modulo xs dp' where
    dp' = V.fromList $ map (\i -> (dp!i + dp!((i - x) `mod` n)) `rem` modulo) [0 .. n - 1]  
    n = V.length dp

solve n modulo = dynamic modulo primes dp where 
    dp = V.fromList (1 : replicate (sum primes) 0)
    primes = primesTo n

main = print $ foldl' helper 0 $ zip [0 .. ] $ V.toList (solve 5000 modulo) where
    helper s (i, ways) = if testPrime i
        then (s + ways) `rem` modulo
        else s
    modulo = 10^16
