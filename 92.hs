import Data.Array
import Data.Char (digitToInt)
import Data.List (foldl')

limit = 1000

endAt89' :: Int -> Bool
endAt89' 89 = True
endAt89' 1 = False
endAt89' x = endAt89 $ next x
    where next x = sum $ map ((^2).digitToInt) $ show x

dp = listArray (1, limit) [ endAt89' x | x <- [1 .. limit] ]

endAt89 :: Int -> Bool
endAt89 x
    | x <= limit = dp!x
    | otherwise = endAt89' x

boolToInt False = 0
boolToInt True = 1

main = print $ foldl' (\s x -> s + (boolToInt $ endAt89 x)) 0 [1 .. 10000000]
