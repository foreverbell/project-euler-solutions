-- run with flags +RTS -K512m -A32m

import qualified Data.Set as S
import Data.List (foldl')
import Data.Array.ST
import Data.Array.IArray ((!), listArray, Array)
import Control.Monad
import Control.Monad.ST

bound = 1000000

isqrt = floor . sqrt . fromIntegral

amicable n = (succ . sum) $ do
    d <- [ 2 .. (isqrt n) ]
    if (n `mod` d == 0)
        then if (d * d == n) 
            then return d
            else return $ d + (n `div` d)
        else return 0

amnext :: [Int]
amnext = 0 : 0 : map amicable [2 .. bound]

cyclen :: [Int]
cyclen = runST $ do
    let amnextArr = listArray (0, (length amnext) - 1) amnext
    arr <- newArray (0, bound) 0 :: ST s (STArray s Int Int)
    forM_ [1 .. bound] $ \n -> do
        old <- readArray arr n
        when (old == 0) $ do
            let res = go amnextArr n S.empty []
            update arr res
    getElems arr
    where
        go :: Array Int Int -> Int -> S.Set Int -> [Int] -> [Int]
        go amnext cur s ret
            | cur > bound = (-1:ret)
            | S.member cur s = (cur:ret)
            | otherwise = go amnext (amnext!cur) (S.insert cur s) (cur:ret)
        setValue :: STArray s Int Int -> [Int] -> Int -> ST s ()
        setValue arr xs val = forM_ xs $ \x -> writeArray arr x val
        update :: STArray s Int Int -> [Int] -> ST s ()
        update arr (-1:xs) = setValue arr xs (-1)
        update arr (x:xs) = do
            setValue arr (x:t1) ((length t1) + 1)
            setValue arr (tail t2) (-1)
            where (t1, t2) = span (/= x) xs

main = print result where
    result = fst $ foldl' cmp (0, 0) $ zip [0 .. ] cyclen
    cmp (a1,v1) (a2,v2) = case compare v1 v2 of
        LT -> (a2, v2)
        _  -> (a1, v1)
		