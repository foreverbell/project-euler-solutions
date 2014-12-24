import Data.List

factorial n = product [1 .. n]

sumOfDF x = sum $ map factorial $ unfoldr helper x
    where
        helper x
            | x == 0 = Nothing
            | otherwise = Just (x `mod` 10, x `div` 10)

main = do
    print $ sum l
        where l = [ x | x <- [10 .. 7*(factorial 9)], x == (sumOfDF x) ]
