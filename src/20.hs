import Data.List

sumOfDigit :: Integer -> Integer
sumOfDigit x = sum $ unfoldr helper x where
    helper 0 = Nothing
    helper x = Just (x `mod` 10, x `div` 10)

main = print $ sumOfDigit $ product [1 .. 100]
