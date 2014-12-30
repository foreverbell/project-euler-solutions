
fromString :: [[String]] -> [[Int]]
fromString s = map (\x -> map (\y -> read y) x) s

dp :: Int -> [[Int]] -> [Int]
dp level triangle
    | level == 0 = row
    | otherwise  = zipWith (+) row (zipWith max (0 : bak) (bak ++ [0]))
    where row = head triangle 
          bak = dp (level - 1) (tail triangle)
      
solve :: String -> Int
solve input = maximum (dp (n - 1) (fromString triangle))
    where triangle = reverse (map words (lines input))
          n = length triangle

main = (readFile "p018_input.txt") >>= (print . solve)
