module Common.Fenwick (
    ask,
    askLR,
    modify
) where

import Data.Bits
import Data.Array
import Data.Array.MArray
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.List (foldl')
import Control.Monad (liftM, liftM2, mapM, forM_)

{-# INLINABLE ask #-}
{-# INLINABLE askLR #-}
{-# INLINABLE modify #-}

ask :: (Num e, MArray a e m) => (e -> e -> e) -> a Int e -> Int -> m e
ask f fenwick 0 = return 0
ask f fenwick x = liftM (foldl' f 0) $ mapM (unsafeRead fenwick) xs where
    xs = takeWhile (> 0) $ x : map (\x -> x - (x .&. (-x))) xs

askLR :: (Num e, MArray a e m) => (e -> e -> e) -> a Int e -> Int -> Int -> m e
askLR f fenwick l r = if (l <= r)
    then liftM2 f (ask f fenwick r) (liftM negate (ask f fenwick (l - 1)))
    else return 0

modify :: (Num e, MArray a e m) => (e -> e -> e) -> a Int e -> Int -> Int -> e -> m ()
modify f fenwick n x d = forM_ xs $ \i -> do 
    v' <- unsafeRead fenwick i
    unsafeWrite fenwick i (f v' d) where
        xs = takeWhile (<= n) $ x : map (\x -> x + (x .&. (-x))) xs

