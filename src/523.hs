import Text.Printf (printf)

-- assume the first n-1 elements are sorted, and the last element is x, 
-- it takes 2^(x-1) steps to sort the whole list.

main = putStrLn $ printf "%.2f" $ sum $ map f [1 .. 30]
  where
    f :: Int -> Double
    f n = (2^(n-1) - 1) / fromIntegral n
