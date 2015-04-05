import Control.Parallel (pseq, par)

solve :: Int -> Int -> Int -> Int
solve n a b = (x' + y' + f) * count * (count + 1) `div` 2 where
    x = a * a
    y = b * b
    f = x * y
    g = (a + b) * (a + b)
    x' = x * g
    y' = y * g
    count = n `div` y'

enumerate :: Int -> (Int, Int) -> (Int, Int) -> Int
enumerate n x@(a, b) y@(c, d) = if (f * f > n) 
    then 0
    else rec1 `par` rec2 `pseq` (cur + rec1 + rec2) where
        z@(e, f) = (a + c, b + d)
        cur = solve n e f
        rec1 = enumerate n x z
        rec2 = enumerate n y z

main = print $ (enumerate n (0, 1) (1, 1)) + (solve n 1 1) where
    n = 10^9
