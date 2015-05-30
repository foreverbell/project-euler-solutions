import Data.Foldable (maximumBy)
import Data.Function (on)
import Common.Numbers.Primes (testPrime)
import Common.List (maximumBy')

produceLength (a, b) = length $ takeWhile (testPrime . (\n -> n^2+a*n+b)) [0 .. ]

main = print $ uncurry (*) result where
    result = maximumBy' (compare `on` produceLength) $ [ (a,b) | a <- [-999 .. 999], b <- [-999 .. 999] ]
