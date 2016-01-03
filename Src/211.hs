
import Common.Util (isqrt)
import Control.Monad (forM_, when)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

eratos :: Int -> UArray Int Int
eratos n = runSTUArray $ do
    isPrime <- newListArray (1, n) (repeat True) :: ST s (STUArray s Int Bool)
    minPrimePart <- newListArray (1, n) [1 .. n] :: ST s (STUArray s Int Int)
    sigma2 <- newListArray (1, n) (repeat 0) :: ST s (STUArray s Int Int)
    writeArray sigma2 1 1
    forM_ [2 .. n] $ \i -> do
        prime <- readArray isPrime i
        when prime $ do
            writeArray sigma2 i $ 1 + i * i
            forM_ [i^2, i^2 + i .. n] $ \j -> do
                prime' <- readArray isPrime j
                when prime' $ do
                    writeArray isPrime j False
                    let q = j `div` i
                    minPrime' <- readArray minPrimePart q
                    if (minPrime' `mod` i == 0)
                        then writeArray minPrimePart j (minPrime' * i)
                        else writeArray minPrimePart j i
                    d' <- readArray minPrimePart j
                    when (d' == j) $ do
                        s <- readArray sigma2 q
                        writeArray sigma2 j $ s * i * i + 1
    forM_ [2 .. n] $ \i -> do
        d1 <- readArray minPrimePart i
        let d2 = i `div` d1
        when (d1 /= i) $ do
            s1 <- readArray sigma2 d1
            s2 <- readArray sigma2 d2
            writeArray sigma2 i (s1 * s2)
    return sigma2

perfect n = root * root == n where
    root = isqrt n

main = print $ sum $ map fst $ filter (perfect . snd) sigma2 where
    sigma2 = assocs $ eratos 64000000

