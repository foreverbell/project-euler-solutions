import Data.List (unfoldr)

sumOfDigit5 = sum . map (^5) . unfoldr helper where
    helper 0 = Nothing
    helper x = Just (x `mod` 10, x `div` 10)

main = print $ sum $ filter (\x -> x == (sumOfDigit5 x)) [2 .. 999999]
