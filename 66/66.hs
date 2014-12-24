import Data.List (maximumBy)
import Data.Function (on)

diophantine n root a b = x : (diophantine n root a' b')
    where
        a' = (n - b * b) `div` a
        x = (root + b) `div` a'
        b' = x * a' - b

solve d = fst $ head $ dropWhile (\(x,y) -> x^2-d*y^2 /= 1) convergent
    where
        cf = diophantine d root 1 root
        convergent = tail $ zip xs ys
        xs = 1 : root : (zipWith3 (\a b c -> a * b + c) cf (tail xs) xs)
        ys = 0 : 1 : (zipWith3 (\a b c -> a * b + c) cf (tail ys) ys)
        isqrt = floor . sqrt . fromIntegral
        root = isqrt d

main = print $ maximumBy (compare `on` solve) nonSquare
    where
        nonSquare = filter (not . isSquared) [1 .. 1000]
        isqrt = floor . sqrt . fromIntegral
        isSquared x = y * y == x
            where y = isqrt x

-- http://en.wikipedia.org/wiki/Pell's_equation
