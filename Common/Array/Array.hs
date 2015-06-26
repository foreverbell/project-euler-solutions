module Common.Array.Array (
    modifyArray
) where

import Data.Array.MArray (MArray, Ix, readArray, writeArray)

{-# INLINABLE modifyArray #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = (writeArray a i) . f =<< readArray a i
