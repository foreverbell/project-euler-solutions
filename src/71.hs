import Data.Ratio
import Data.List (foldl1')
import Common.List (maximum')

calculate d = n % d where 
    n = d * 3 `div` 7

main = print $ numerator (maximum' [ calculate d | d <- [1 .. 1000000], d `mod` 7 /= 0 ])
