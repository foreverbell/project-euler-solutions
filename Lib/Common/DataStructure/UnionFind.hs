{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}

module Common.DataStructure.UnionFind (
  UFSet
, make
, find
, union
) where

import           Control.Monad (liftM2)
import           Control.Monad.Primitive
import qualified Common.MonadRef as R
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data PrimMonad m => UFSet m = UFSet {
  ufsSize :: R.Ref m Int,
  ufsSet :: MV.MVector (PrimState m) Int
}

make :: (PrimMonad m, R.MonadRef m) => Int -> m (UFSet m) 
make n = liftM2 UFSet (R.new n) (V.thaw $ V.fromList [0 .. n - 1])

find :: (PrimMonad m) => UFSet m -> Int -> m Int
find ufs u = do
  f <- MV.unsafeRead (ufsSet ufs) u
  if u == f
    then return u
    else do
      f' <- find ufs f
      MV.unsafeWrite (ufsSet ufs) u f'
      return f'

union :: (PrimMonad m, R.MonadRef m) => UFSet m -> Int -> Int -> m Bool
union ufs u v = do
  u' <- find ufs u
  v' <- find ufs v
  if u' /= v'
    then do
      MV.unsafeWrite (ufsSet ufs) u' v' 
      R.modify_' (ufsSize ufs) pred
      return True
    else return False
