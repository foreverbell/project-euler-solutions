import Data.List
import Data.Char

solve :: String -> Int
solve input = length triangleWord
    where words' = filter (\s -> (length s) >= 2) (groupBy (\a b -> ((a == ',') == (b == ','))) input)
          words  = sort $ map (tail . init) words'
          wordValue s = sum $ map (\c -> ord c - ord 'A' + 1) s
          triangleNumber = [ (n * (n + 1)) `div` 2 | n <- [1 .. 100] ]
          triangleWord   = [ x | x <- words, (wordValue x) `elem` triangleNumber ]

main = readFile "p042_words.txt" >>= (print . solve)
