
triangle = [ n * (n + 1) `div` 2 | n <- [1 .. ] ]
pentagonal = [ n * (3 * n - 1) `div` 2 | n <- [1 .. ] ]
hexagonal = [ n * (2 * n - 1) | n <- [1 .. ] ] 

intersect xs'@(x:xs) ys'@(y:ys) = case (compare x y) of
    EQ -> x : (intersect xs ys)
    LT -> intersect xs ys'
    GT -> intersect xs' ys

main = do
    print $ is !! 2
    where is = intersect triangle $ intersect pentagonal hexagonal
