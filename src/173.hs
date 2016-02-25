import Common.Utils (isqrt)

root d | d <= 0 = 0
       | otherwise = isqrt d

solve :: Int -> Int
solve n = sum [ count a | a <- [1 .. n `div` 4 + 1] ]
  where
    count a = (a - k) `div` 2
      where k = 1 + (isqrt $ a*a - n - 1)

main = print $ solve 1000000
