import Data.Ratio
import Data.List (sort, nub, maximumBy)
import Data.Function (on)

ratio2Integer :: (Ratio Integer) -> Integer
ratio2Integer r 
    | a `mod` b == 0 = a `div` b
    | otherwise = -1 
    where
        a = numerator r
        b = denominator r

solve :: [Integer] -> Integer
solve xs = getConsecutive $ concat [ dfs (kill n rs) (Just (rs !! n)) | n <- [0 .. 3] ] where
    kill n xs = (take n xs) ++ (drop (n + 1) xs)
    rs = map (\x -> x % 1) xs
    dfs :: [Ratio Integer] -> Maybe (Ratio Integer) -> [Maybe (Ratio Integer)]
    dfs _ Nothing = []
    dfs [] value  = [value]
    dfs can value = concat ret where
        ret = do
            useIndex <- [0 .. (pred . length) can]
            let newCan = kill useIndex can
            op <- [1 .. 6]
            let newValue = value >>= (apply op (can !! useIndex)) 
            return $ dfs newCan newValue
        apply 1 x y = Just $ x + y
        apply 2 x y = Just $ x - y
        apply 3 x y = Just $ y - x
        apply 4 x y = Just $ x * y
        apply 5 x 0 = Nothing
        apply 5 x y = Just $ x / y
        apply 6 0 y = Nothing
        apply 6 x y = Just $ y / x

getConsecutive :: [Maybe (Ratio Integer)] -> Integer
getConsecutive rs = pred $ snd $ head $ dropWhile (\(x, y) -> x == y) $ zip xs [1 .. ] where
    xs :: [Integer]
    xs = dropWhile (<= 0) $ nub . sort $ map (\(Just x) -> ratio2Integer x) $ filter (\x -> x /= Nothing) rs

main = print $ helper 0 $ snd $ maximumBy (compare `on` fst) [ (solve x, x) | x <- comb4 ] where
    comb4 = do
        a <- [1 .. 9]
        b <- [a + 1 .. 9]
        c <- [b + 1 .. 9]
        d <- [c + 1 .. 9]
        return [a, b, c, d]
    helper ret [] = ret
    helper ret (x:xs) = helper (ret * 10 + x) xs
