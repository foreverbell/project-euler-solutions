-- <=> find minimal positive integer x satifying 10^x=1 (mod d)

import Data.List
import Data.Function (on)

cycleLength :: Int -> Int

cycleLength n
    | n `mod` 2 == 0 || n `mod` 5 == 0 = 0
    | otherwise = 1 + length (unfoldr helper (10 `mod` n))
    where
        helper x
            | x == 1 = Nothing
            | otherwise = Just (x, (x * 10) `mod` n)

main = print $ maximumBy (compare `on` cycleLength) [3, 5 .. 999]
