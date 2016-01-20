module Common.DataStructure.Fenwick (
  Fenwick
, make
, ask
, askLR
, modify
) where

import           Control.Monad (liftM, liftM2, forM_)
import           Control.Monad.Primitive
import           Data.Bits ((.&.))
import           Data.List (foldl')
import qualified Data.Vector.Unboxed.Mutable as MV

newtype (PrimMonad m, Num e, MV.Unbox e) => Fenwick m e = Fenwick (MV.MVector (PrimState m) e)

{-# INLINABLE make #-}
{-# INLINABLE ask #-}
{-# INLINABLE askLR #-}
{-# INLINABLE modify #-}

make :: (PrimMonad m, Num e, MV.Unbox e) => Int -> m (Fenwick m e)
make n = liftM Fenwick (MV.replicate (n + 1) 0)

ask :: (PrimMonad m, Num e, MV.Unbox e) => Fenwick m e -> (e -> e -> e) -> Int -> m e
ask _ _ 0 = return 0
ask (Fenwick fenwick) f x = liftM (foldl' f 0) $ mapM (MV.unsafeRead fenwick) xs 
  where
    xs = takeWhile (> 0) $ iterate (\x -> x - (x .&. (-x))) x

askLR :: (PrimMonad m, Num e, MV.Unbox e) => Fenwick m e -> (e -> e -> e) -> Int -> Int -> m e
askLR fenwick f l r 
  | l <= r = liftM2 f (ask fenwick f r) (liftM negate (ask fenwick f (l - 1)))
  | otherwise = return 0

modify :: (PrimMonad m, Num e, MV.Unbox e) => Fenwick m e -> (e -> e -> e) -> Int -> e -> m ()
modify (Fenwick fenwick) f x d = forM_ xs $ \i -> MV.unsafeModify fenwick (f d) i
  where
    xs = takeWhile (<= n) $ iterate (\x -> x + (x .&. (-x))) x
    n = MV.length fenwick - 1
