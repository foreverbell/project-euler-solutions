import Data.List (unfoldr, reverse)
import Data.Char (chr, ord)

decToBin :: Int -> String
decToBin x = reverse $ unfoldr helper x where
    helper 0 = Nothing
    helper x = Just (chr ((ord '0') + (x `mod` 2)), x `div` 2)

palindromic xs = xs == reverse xs

main = print $ sum $ [ x | x <- [1 .. 999999], palindromic (show x), palindromic (decToBin x) ]
