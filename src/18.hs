
fromString :: [[String]] -> [[Int]]
fromString s = map (\x -> map (\y -> read y) x) s

dp :: Int -> [[Int]] -> [Int]
dp 0 triangle = head triangle
dp level triangle = zipWith (+) row (zipWith max (0 : bak) (bak ++ [0]))
    where row = head triangle 
          bak = dp (level - 1) (tail triangle)
      
solve :: String -> Int
solve input = maximum (dp (n - 1) (fromString triangle))
    where triangle = reverse (map words (lines input))
          n = length triangle

main = (readFile "input/p018_input.txt") >>= (print . solve)
