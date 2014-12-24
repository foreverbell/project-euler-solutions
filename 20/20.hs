import Data.List

sumOfDigit :: Integer -> Integer
sumOfDigit x = sum digits
    where digits = unfoldr helper x
          helper x
            | x == 0 = Nothing
            | otherwise = Just (x `mod` 10, x `div` 10)

main = do
    print $ sumOfDigit $ product [1 .. 100]
