
import Common.Numbers (powMod, inverse)
import Data.List (foldl')

solve :: Int
solve = foldl' helper 0 $ -n : (map count [1 .. n]) where
    modulo = 10^9+7
    n = 10^6
    count k = ((powMod ((1 - k * k) `mod` modulo) (n + 1) modulo) + (modulo - 1)) * (inverse k modulo) `mod` modulo * (inverse (-k) modulo) `mod` modulo
    helper x y = (x + y) `mod` modulo

main = print solve
