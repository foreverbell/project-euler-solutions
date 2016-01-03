module Common.Array.Unboxed.Vector (
    modify,
    unsafeModify
) where

import Prelude hiding (read, write)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Unboxed.Mutable (Unbox, MVector, read, write, unsafeRead, unsafeWrite)

{-# INLINABLE modify #-}
{-# INLINABLE unsafeModify #-}

modify :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify a i f = write a i . f =<< read a i

unsafeModify :: (PrimMonad m, Unbox a) => MVector (PrimState m) a -> Int -> (a -> a) -> m ()
unsafeModify a i f = unsafeWrite a i . f =<< unsafeRead a i

