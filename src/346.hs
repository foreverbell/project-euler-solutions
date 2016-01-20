import Common.Utils (isqrt)
import Common.List (nub')

n = 10^12 :: Int
root = isqrt n

get :: Int -> [Int]
get m = tail $ takeWhile (<= n) $ go (m + 1)
  where
    go x = x : go (x * m + 1)

repunits :: [Int]
repunits = 1 : nub' (concatMap get [2 .. root])

main = print $ sum repunits
