import Data.Char (digitToInt)

solve input = maximum $ rec 12 digits where
    digits = map digitToInt $ concat $ words input
    rec 0 xs = xs
    rec n xs = zipWith (*) xs $ rec (n - 1) (tail xs)

main = (print . solve) =<< readFile "input/p008_input.txt"
