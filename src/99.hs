import Data.List (maximumBy)
import Data.Function (on)

splitByComma :: String -> (Int, Int)
splitByComma s = (read a, read (tail b))
    where (a, b) = span (\x -> x /= ',') s

readInput :: IO [(Int, Int)]
readInput = readFile "input/p099_base_exp.txt" >>= (return . map splitByComma . words)

solve :: [(Int, Int)] -> Int
solve xs = fst $ maximumBy (compare `on` value) (zip [1 .. ] xs)
    where value (index, (a, b)) = (fromIntegral b) * (log (fromIntegral a))

main = readInput >>= (print . solve)
