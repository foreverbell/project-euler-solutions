import qualified Data.MemoCombinators as Memo
import           Text.Printf (printf)

b :: Int -> Double
b n = Memo.bits b' n
  where
    b' 1 = 1.0
    b' 2 = 1.5
    b' n = 1 + ((b x) * fromIntegral x + (b y) * fromIntegral y) / fromIntegral n
      where x = (n - 1) `div` 2
            y = n `div` 2

-- https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant#Asymptotic_expansions
h :: Int -> Double
h n | n < 100 = sum $ map (\x -> 1.0 / fromIntegral x) [1 .. n]
    | otherwise = gamma + log n1 + 1 / 2 / n1 - 1 / 12 / n2 + 1 / 120 / n4
  where
    gamma = 0.5772156649
    n1 = fromIntegral n :: Double
    n2 = n1 * n1
    n4 = n2 * n2

r :: Int -> Double
r n = 2 * (h n) * (n1 + 1) / n1 - 3
  where n1 = fromIntegral n :: Double

main = putStrLn $ printf "%.8f" (r n - b n)
  where n = 10^10
