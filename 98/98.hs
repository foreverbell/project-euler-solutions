import Data.List (groupBy, sort)
import Data.Char (ord)
import Control.Monad (guard)
import Data.Array.IArray

readInput :: IO [String]
readInput = (readFile "p098_words.txt") >>= (return . foo) where 
    foo input = map bar $ filter (\s -> (length s) >= 2) $ groupBy (\a b -> ((a == ',') == (b == ','))) input
    bar (x:xs) = take ((length xs) - 1) xs

squares :: [Int]
squares = [ n*n | n <- [1 .. ] ]

isSquared x = (root * root) == x where 
    isqrt = floor . sqrt . fromIntegral
    root = isqrt x

anagram word1 word2 = (sort word1) == (sort word2)

score word1 word2 = maximum (0 : ret) where
    l = length word1
    ub = (10 ^ l) - 1
    lb = 10 ^ (l - 1)
    sqr = takeWhile (\x -> x <= ub) $ dropWhile (\x -> x < lb) squares
    check x = and $ do
        i <- [0 .. l - 1]
        j <- [i + 1 .. l - 1]
        return $ (s!!i == s!!j && word1!!i == word1!!j) || (s!!i /= s!!j && word1!!i /= word1!!j)
        where s = show x
    replace x = y where
        s = show x
        mapping = zip word1 s
        y = rec word2 0
        rec [] ret = ret
        rec (x:xs) ret = rec xs $ ret * 10 + (find mapping x)
    find (m:ms) x = case compare (fst m) x of
        EQ -> ord (snd m) - (ord '0')
        _ -> find ms x
    ret = do
        x <- sqr
        guard $ check x
        let y = replace x
        guard $ length (show y) == l
        guard $ isSquared y
        return $ max x y

solve :: [String] -> Int
solve s = maximum $ do
    i <- [1 .. n]
    j <- [i + 1 .. n]
    guard $ anagram (arr!i) (arr!j)
    return $ score (arr!i) (arr!j)
    where 
        n = length s
        arr = listArray (1, n) s :: Array Int String

main = readInput >>= (return . solve)
