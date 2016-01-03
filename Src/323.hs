import Common.Numbers.Numbers (binomial)
import Text.Printf (printf)

f :: [Double]
f = 0 : map g [1 .. 32]
  where
    g :: Integer -> Double
    g n = b / (a - 1)
      where 
        a, b :: Double
        a = 2.0 ** (fromIntegral n)
        b = sum [ fromIntegral (binomial n k) * (f !! fromIntegral k) | k <- [0 .. n - 1] ] + a

main = putStrLn $ printf "%.10f" (f !! 32)
