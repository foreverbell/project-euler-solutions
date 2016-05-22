count :: Int -> Int
count 0 = 0
count n = (n `div` 2) + count (n `div` 2)

-- solve (4*n)
solve :: Int -> Int
solve n = (m + 1) * (n + count (n+1))
  where m = 904961

main = print $ solve (10^12 `div` 4)
