import Data.Char

solve input = maximum $ rec 12 digits where
    digits = map digitToInt $ concat $ words input
    rec 0 xs = xs
    rec n xs = zipWith (*) xs $ rec (n - 1) (tail xs)

main = (print . solve) =<< (readFile "Input/p008_input.txt")
