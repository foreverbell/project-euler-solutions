import Data.List
import Data.Char

scoreOfName :: String -> Int
scoreOfName s = foldl' (\n c -> n + (ord c - ord 'A' + 1)) 0 s

solve :: String -> Int
solve input = score
    where names' = filter (\s -> (length s) >= 2) (groupBy (\a b -> ((a == ',') == (b == ','))) input)
          names  = sort $ map (tail . init) names'
          pairs  = zip names [1 .. ]
          score  = sum $ map (\(n, i) -> (scoreOfName n) * i) pairs

main = (readFile "Input/p022_names.txt") >>= (print . solve)
