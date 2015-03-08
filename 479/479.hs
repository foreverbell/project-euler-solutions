
import Data.List (foldl')

powMod a p m = helper a p 1 m where
    helper a 0 r m = r
    helper a p r m = helper (mulMod a a m) (p `div` 2) r' m where
        r' = if (odd p) then mulMod r a m else r
        mulMod a b m = (a * b) `mod` m

inverse x m = powMod x (m - 2) m

solve :: Int
solve = foldl' helper 0 $ -n : (map count [1 .. n]) where
    modulo = 10^9+7
    n = 10^6
    count k = ((powMod ((1 - k * k) `mod` modulo) (n + 1) modulo) + (modulo - 1)) * (inverse k modulo) `mod` modulo * (inverse (-k) modulo) `mod` modulo
    helper x y = (x + y) `mod` modulo

main = print solve
