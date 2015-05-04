import Data.Ratio
import Data.List (foldl1')

calculate d = n % d where 
    n = d * 3 `div` 7

maximum' = foldl1' max  -- strict version

main = print $ numerator (maximum' [ calculate d | d <- [1 .. 1000000], d `mod` 7 /= 0 ])
