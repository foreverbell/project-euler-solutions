import Data.Ratio
import Data.Char (ord)

main = print $ sumOfDigit $ numerator answer where
    sumOfDigit x = sum $ map (\c -> ord c - ord '0') (show x)
    eTerm = 2 : concatMap (\x -> [1, x, 1]) [2, 4 .. ]
    answer = 1 / (foldr (\x y -> 1 / (x + y)) (0 % 1) (take 100 eTerm))
