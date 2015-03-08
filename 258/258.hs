
import Control.Monad (forM_, when)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

modulo :: Int
modulo = 20092010

sumMod :: Int -> Int -> Int
sumMod a b = (a + b) `mod` modulo

mulMat :: Int -> V.Vector Int -> V.Vector Int -> V.Vector Int
mulMat k a b = V.create $ do
    ret <- MV.replicate k 0
    forM_ [0 .. k - 1] $ \i -> do
        when ((a!i) /= 0) $ do
            forM_ [0 .. k - 1] $ \j -> do
                let v = (a!i) * (b!j)
                if (i + j >= k)
                    then (update ret (i + j - k) v) >> (update ret (i + j - k + 1) v)
                    else update ret (i + j) v
    return ret where
        update vec i d = do
            v <- MV.unsafeRead vec i
            MV.unsafeWrite vec i $ sumMod v d 

powMat :: Int -> Int -> V.Vector Int -> V.Vector Int
powMat k p x = unit `seq` x `seq` helper k p x unit where
    unit = V.fromList $ (1 : (replicate (k - 1) 0))
    helper k 0 a r = r
    helper k p a r = a' `seq` r' `seq` helper k (p `div` 2) a' r' where
        a' = mulMat k a a
        r' = if (odd p) 
                then mulMat k r a
                else r

solve :: Int -> [Int] -> [Int] -> Int -> Int
solve k a f n = foldl sumMod 0 $ zipWith (*) f (V.toList power) where
    b = V.fromList $ reverse a 
    baseMat = V.fromList $ 0 : 1 : (replicate (k - 2) 0)
    power = powMat k n baseMat
    
main = print $ solve 2000 a f (10^18) where
    a = (replicate 1998 0) ++ [1, 1]
    f = replicate 2000 1

