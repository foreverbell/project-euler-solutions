-- <=> find minimal positive integer x satifying 10^x=1 (mod d)

import Data.List (unfoldr, maximumBy)
import Data.Function (on)

cycleLength :: Int -> Int
cycleLength n = 1 + length (unfoldr helper (10 `mod` n)) where
    helper 1 = Nothing
    helper x = Just (x, (x * 10) `mod` n)

main = print $ maximumBy (compare `on` cycleLength) $ filter (\x -> x `mod` 2 /= 0 && x `mod` 5 /= 0) [3 .. 999]
