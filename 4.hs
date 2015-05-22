import Data.List (sort, reverse)
import Common.List (nub')

main = print $ head $ filter palindromic $ (reverse . sort . nub') [ x * y | x <- [100 .. 999], y <- [100 .. 999] ] where
    palindromic x = s == (reverse s) where
        s = show x
