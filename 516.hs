
import Common.Primes (testPrime)
import Control.Monad (foldM)
import Data.List (sort)

genBase :: Int -> [Int]
genBase n = do
    a <- takeWhile (\x -> x <= n) as
    b <- takeWhile (\x -> x * a <= n) bs
    c <- takeWhile (\x -> x * a * b <= n) cs 
    return $ a * b * c where
        as = 1 : map (* 2) as
        bs = 1 : map (* 3) bs
        cs = 1 : map (* 5) cs

solve :: Int -> Int
solve n = (sum (map count v1)) `mod` (2^32) where
    v1 = genBase n
    v2 = map (+ 1) $ filter (\x -> x /= 1 && x /= 2 && x /= 4 && (testPrime (x + 1))) v1
    v3 = sort $ foldM helper 1 v2 where
        helper a b = if a <= (n `div` b)
            then [a, a * b]
            else [a]
    count x = (sum (takeWhile (\y -> y <= (n `div` x)) v3)) * x

main = print $ solve (10^12)
