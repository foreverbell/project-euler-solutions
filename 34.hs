import Common.Numbers (factorial)
import Data.List (unfoldr)

sumOfDF x = sum $ map factorial $ unfoldr helper x where
    helper 0 = Nothing
    helper x = Just (x `mod` 10, x `div` 10)

main = print $ sum l where 
    l = [ x | x <- [10 .. 7*(factorial 9)], x == (sumOfDF x) ]
