
-- Almost 3 roots.
-- Estimate the middle result in allowance to give a result when evaluating the polynomial via Horner's rule.

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (when, forM_, guard)
import Control.Monad.ST
import Prelude hiding (read)

n = 16
r = 9 * (n `div` 2)

write :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
write = writeArray

read :: (MArray a e m, Ix i) => a i e -> i -> m e
read = readArray

add arr ix v = do
    v' <- read arr ix
    write arr ix (v + v')

count r1 r2 r3 mu = runST $ do
    dp <- newArray ((0, -r, -r, -r), (n, r, r, r)) 0 :: ST s (STUArray s (Int, Int, Int, Int) Int)
    write dp (n, 0, 0, 0) 1
    forM_ [n, n - 1 .. 1] $ \i -> do
        forM_ [-r .. r] $ \a -> do
            forM_ [-r .. r] $ \b -> do
                forM_ [-r .. r] $ \c -> do
                    v <- read dp (i, a, b, c)
                    when (v /= 0) $ do
                        let digits = if (i == 1) then [1 .. 9] else [0 .. 9]
                        forM_ digits $ \d -> do
                            let a' = a * (-r1) + d
                            let b' = b * (-r2) + d
                            let c' = c * (-r3) + d
                            when (between (-r, r) a' b' c') $ do
                                add dp (i - 1, a', b', c') v
    ret <- read dp (0, 0, 0, 0)
    return $ mu * ret 
    where
        between (l, r) a b c = (between' a) && (between' b) && (between' c) where
            between' a = (l <= a) && (a <= r)

solve = 10^(n-1) + s1 + s2 + s3 where
    s1 = sum $ do
        r1 <- [1 .. 9]
        r2 <- [r1+1 .. 9]
        r3 <- [r2+1 .. 9]
        guard (r1*r2*r3 <= 9)
        return $ count r1 r2 r3 1
    s2 = sum $ do
        r1 <- [1 .. 9]
        r2 <- [r1+1 .. 9]
        guard (r1*r2 <= 9)
        return $ count r1 r1 r2 (-1)
    s3 = sum $ do
        r <- [1 .. 9]
        return $ count r r r 1

main = print $ solve
