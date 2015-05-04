import Data.List (maximumBy)
import Data.Function (on)

isqrt = floor . sqrt . fromIntegral
isSquared x = (isqrt x) ^ 2 == x

diophantine d = filter (\(x,y) -> x^2-d*y^2 == 1) convergent where
    root = isqrt d
    cf = helper d root 1 root
    convergent = tail $ zip xs ys
    xs = 1 : root : (zipWith3 (\a b c -> a * b + c) cf (tail xs) xs)
    ys = 0 : 1 : (zipWith3 (\a b c -> a * b + c) cf (tail ys) ys)
    helper n root a b = x : (helper n root a' b') where
        a' = (n - b * b) `div` a
        x = (root + b) `div` a'
        b' = x * a' - b

main = print $ maximumBy (compare `on` (fst . head . diophantine)) nonSquare where
    nonSquare = filter (not . isSquared) [1 .. 1000]

-- http://en.wikipedia.org/wiki/Pell's_equation
