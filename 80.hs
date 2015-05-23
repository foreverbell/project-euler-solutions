
import Data.List ((\\))
import Data.Char (digitToInt)
import Common.Util (isqrt)

go :: Integer -> Int
go number = sumOfDigit100 decimial where
    root = isqrt number
    target = number * 10^200
    bsearch :: Integer -> Integer -> Integer
    bsearch l r = if l == r
        then l
        else case compare (mid*mid) target of
            EQ -> mid
            LT -> bsearch mid r
            GT -> bsearch l (mid - 1)
        where mid = 1 + (l + r) `div` 2
    decimial = bsearch (root*10^100) ((root+1)*10^100)
    sumOfDigit100 x = sum $ map digitToInt (take 100 $ show x)

main = print $ sum [ go x | x <- nonSquare ] where
    nonSquare = [1 .. 100] \\ [ i*i | i <- [1 .. 10] ]
