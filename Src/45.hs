
import Common.List (intersect)

triangle = [ n * (n + 1) `div` 2 | n <- [1 .. ] ]
pentagonal = [ n * (3 * n - 1) `div` 2 | n <- [1 .. ] ]
hexagonal = [ n * (2 * n - 1) | n <- [1 .. ] ] 

main = print $ is !! 2
    where is = triangle `intersect` (pentagonal `intersect` hexagonal)
