module Common.Array.Vector (
    modify,
    unsafeModify
) where

import Prelude hiding (read)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Mutable (MVector, read, write, unsafeRead, unsafeWrite)

{-# INLINABLE modify #-}
{-# INLINABLE unsafeModify #-}

modify :: PrimMonad m => MVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify a i f = write a i . f =<< read a i

unsafeModify :: PrimMonad m => MVector (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify a i f = unsafeWrite a i . f =<< unsafeRead a i

