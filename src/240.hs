import Common.Numbers.Numbers (multiBinomial)
import Data.List (group)

g d f | d == 0 = [[]]
      | otherwise = concatMap (\f0 -> map (f0:) (g (d - 1) f0)) [1 .. f]

combinations :: Int -> Int -> Int -> Int -> [[Int]]
combinations d f top s = concatMap r possibles
  where
    possibles = filter (\xs -> sum xs == s) $ g top f :: [[Int]]
    r xs = map (\ys -> xs ++ ys) $ g (d - top) (last xs)

solve d f top s = sum $ map count combs
  where
    combs = combinations d f top s
    count xs = multiBinomial $ map length (group xs)

main = print $ solve 20 12 10 70
