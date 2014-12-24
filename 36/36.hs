import Data.List
import Data.Char

decToBin :: Int -> String
decToBin x = reverse $ unfoldr helper x
    where
        helper x
            | x == 0 = Nothing
            | otherwise = Just (chr ((ord '0') + (x `mod` 2)), x `div` 2)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs)
    | x == y = isPalindrome $ init xs
    | otherwise = False
    where y = last xs

main = do
    print $ sum candidate
    where
        candidate = [ x | x <- [1 .. 999999], isPalindrome $ show x, isPalindrome $ decToBin x ]
