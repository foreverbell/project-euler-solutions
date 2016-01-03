
import Common.Util (if')
import Common.Numbers.Numbers (inverseTo)
import Common.Numbers.Primes (testPrime)
import Data.Array.Unboxed
import Data.List (foldl')
import Control.DeepSeq (deepseq)

infixl 6 .+.
(.+.) a b = (a + b) `rem` modulo 
infixl 7 .*.
(.*.) a b = (a * b) `rem` modulo 
modulo = 1000000007 :: Int

facts :: UArray Int Int
facts = listArray (0, n) facts' where
    facts' = 1 : zipWith (.*.) facts' [1 .. n]
    n = 11111111

invfacts :: UArray Int Int
invfacts = invs `deepseq` listArray (0, n) invfacts' where
    invs = drop 1 $ inverseTo n modulo
    invfacts' = 1 : zipWith (.*.) invfacts' invs
    n = 11111111

binomial :: Int -> Int -> Int
binomial n m = if' (m < 0 || n < m) 0 $ a .*. b .*. c where
    a = facts ! n
    b = invfacts ! m
    c = invfacts ! (n - m)

count p = foldl' (.+.) 0 [ helper i | i <- [0 .. (floor root)] ] where
    c = binomial
    root = sqrt (fromIntegral p) :: Double
    helper k1 = (((k1 + k3 - 1) `c` (k3 - 1)) + ((k1 + k2) `c` k2) - ((k1 + k3 - 1) `c` (k3 - 1))) `mod` modulo where
        k2 = floor $ (fromIntegral p) - (fromIntegral k1) * root
        k3 = max 0 $ floor $ (fromIntegral p) - (fromIntegral (k1 + 1)) * root + 1
        
main = print $ foldl' (\a b -> a .+. (count b)) 0 $ filter testPrime [111 .. 10010000]
