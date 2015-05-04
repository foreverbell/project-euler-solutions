
import Control.Monad (forM_, mapM, liftM, liftM2, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST
import Data.List (zipWith4, foldl')
import Data.Bits 

askSum :: (Ix i, Bits i, Num i, Num e, MArray a e m) => (e -> e -> e) -> a i e -> Int -> m e
askSum f fenwick 0 = return 0
askSum f fenwick x = liftM (foldl' f 0) $ mapM (unsafeRead fenwick) xs where
    xs = takeWhile (> 0) $ x : map (\x -> x - (x .&. (-x))) xs

askSumLR :: (Ix i, Bits i, Num i, Num e, MArray a e m) => (e -> e -> e) -> a i e -> Int -> Int -> m e
askSumLR f fenwick l r = if (l <= r)
    then liftM2 f (askSum f fenwick r) (liftM negate (askSum f fenwick (l - 1)))
    else return 0

modify :: (Ix i, Bits i, Num i, Num e, MArray a e m) => (e -> e -> e) -> a i e -> Int -> Int -> e -> m ()
modify f fenwick n x d = forM_ xs $ \i -> do 
    v' <- unsafeRead fenwick i
    unsafeWrite fenwick i (f v' d) where
        xs = takeWhile (<= n) $ x : map (\x -> x + (x .&. (-x))) xs

m = 10^18
foldMod a b = (a + b) `mod` m

eratos :: Int -> UArray Int Int
eratos n = runSTUArray $ do
    isPrime <- newListArray (1, n) (repeat True) :: ST s (STUArray s Int Bool)
    minPrimePart <- newListArray (1, n) [1 .. n] :: ST s (STUArray s Int Int)
    sumOfDivisors <- newListArray (1, n) (repeat 2) :: ST s (STUArray s Int Int)
    writeArray sumOfDivisors 1 1
    forM_ [2 .. n] $ \i -> do
        prime <- readArray isPrime i
        when prime $ do
            forM_ [i^2, i^2 + i .. n] $ \j -> do
                prime' <- readArray isPrime j
                when prime' $ do
                    writeArray isPrime j False
                    let q = j `div` i
                    minPrime' <- readArray minPrimePart q
                    if (minPrime' `mod` i == 0)
                        then writeArray minPrimePart j (minPrime' * i)
                        else writeArray minPrimePart j i
                    d1 <- readArray minPrimePart j
                    when (d1 == j) $ do
                        s <- readArray sumOfDivisors q
                        writeArray sumOfDivisors j (s + 1)
    forM_ [2 .. n] $ \i -> do
        d1 <- readArray minPrimePart i
        let d2 = i `div` d1
        when (d1 /= i) $ do
            s1 <- readArray sumOfDivisors d1
            s2 <- readArray sumOfDivisors d2
            writeArray sumOfDivisors i (s1 * s2)
    return sumOfDivisors

buildDT :: Int -> UArray Int Int
buildDT n = runSTUArray $ do
    ret <- newListArray (1, n) (repeat 0)
    let d = eratos (n + 1)
    forM_ [1, 3 .. n] $ \i -> writeArray ret i $ (d!i) * (d!((i + 1) `div` 2))
    forM_ [2, 4 .. n] $ \i -> writeArray ret i $ (d!(i `div` 2)) * (d!(i + 1))
    return ret

solve :: Int -> Int
solve n = result where
    dT = buildDT n
    maxd = foldl' (\r i -> max r (dT!i)) 0 [1 .. n]
    result = runST $ do
        dp1 <- newArray (0, maxd) 0 :: ST s (STUArray s Int Int)
        dp2 <- newArray (0, maxd) 0 :: ST s (STUArray s Int Int)
        dp3 <- newArray (0, maxd) 0 :: ST s (STUArray s Int Int)
        forM_ [1 .. n] $ \i -> do
            let d = dT!i
            modify foldMod dp1 maxd d 1
            (askSumLR foldMod dp1 (d + 1) maxd) >>= (modify foldMod dp2 maxd d)
            (askSumLR foldMod dp2 (d + 1) maxd) >>= (modify foldMod dp3 maxd d)
        askSum foldMod dp3 maxd

main = print $ solve 60000000
