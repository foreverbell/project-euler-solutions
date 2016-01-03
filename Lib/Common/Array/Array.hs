module Common.Array.Array (
    modifyArray,
    unsafeModify
) where

import GHC.Arr
import Data.Array.MArray (MArray, readArray, writeArray)
import Data.Array.Base (unsafeRead, unsafeWrite)

{-# INLINABLE modifyArray #-}
{-# INLINABLE unsafeModify #-}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = writeArray a i . f =<< readArray a i

unsafeModify :: (MArray a e m, GHC.Arr.Ix i) => a i e -> Int -> (e -> e) -> m () 
unsafeModify a i f = unsafeWrite a i . f =<< unsafeRead a i

