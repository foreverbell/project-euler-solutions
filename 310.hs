
import Data.Bits (xor)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.List (sort, nub)

grundy :: Int -> UArray Int Int
grundy n = runSTUArray $ do
    ret <- newArray (0, n) 0
    forM_ [1 .. n] $ \i -> do
        sgs <- forM [1 .. (isqrt i)] $ \j -> readArray ret (i - j * j)
        writeArray ret i (mex sgs)
    return ret
    where isqrt = floor . sqrt . fromIntegral
          mex xs = if null xs'' 
            then length xs'
            else fst $ head xs''
              where
                xs' = zip [0 .. ] $ (nub . sort) xs
                xs'' = dropWhile (\(x, y) -> x == y) xs'
                
solve n = answer where
    sg = grundy n
    bound = maximum (elems sg)
    count :: UArray Int Int
    count = runSTUArray $ do
        ret <- newArray (0, bound) 0 
        forM_ [0 .. n] $ \i -> do
            v <- readArray ret (sg!i)
            writeArray ret (sg!i) (v + 1)
        return ret
    a = sum $ do
        i <- [0 .. bound]
        j <- [0 .. bound]
        k <- [0 .. bound]
        guard $ (i `xor` j `xor` k) == 0
        return $ (count!i) * (count!j) * (count!k)
    b = count!0
    c = count!0 * n 
    answer = (a + 5 * b + 3 * c) `div` 6

main = print $ solve 100000
