
import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Base (unsafeRead, unsafeWrite)

modulo :: Int
modulo = 20092010

sumMod :: [Int] -> Int
sumMod xs = foldl helper 0 xs where
    helper accum x = (accum + x) `mod` modulo

mulMat :: Int -> UArray Int Int -> UArray Int Int -> UArray Int Int
mulMat k a b = runSTUArray $ do
    ret <- newArray (0, k - 1) 0
    forM_ [0 .. k - 1] $ \i -> do
        when ((a!i) /= 0) $ do
            forM_ [0 .. k - 1] $ \j -> do
                let v = (a!i) * (b!j)
                if (i + j >= k)
                    then (addArray ret (i + j - k) v) >> (addArray ret (i + j - k + 1) v)
                    else addArray ret (i + j) v
    return ret where 
        addArray a i d = do
            v <- unsafeRead a i -- v <- readArray a i
            unsafeWrite a i $ (v + d) `mod` modulo -- writeArray a i $ (v + d) `mod` modulo

powMat :: Int -> Int -> UArray Int Int -> UArray Int Int
powMat k p x = unit `seq` x `seq` helper k p x unit where
    unit = listArray (0, k - 1) $ (1 : (replicate (k - 1) 0))
    helper k 0 a r = r
    helper k p a r = a' `seq` r' `seq` helper k (p `div` 2) a' r' where
        a' = mulMat k a a
        r' = if (odd p) 
                then mulMat k r a
                else r

solve :: Int -> [Int] -> [Int] -> Int -> Int
solve k a f n = sumMod $ zipWith (*) f (elems power) where
    b = listArray (0, k - 1) $ reverse a :: UArray Int Int
    baseMat = listArray (0, k - 1) $ 0 : 1 : (repeat 0)
    power = powMat k n baseMat
    
main = print $ solve 2000 a f (10^18) where
    a = (replicate 1998 0) ++ [1, 1]
    f = replicate 2000 1

