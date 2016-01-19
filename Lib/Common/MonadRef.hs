{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Common.MonadRef (
  MonadRef (..)
) where

import Control.Monad.ST (ST)
import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef, modifyIORef')
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef')

import Prelude hiding (read)

class Monad m => MonadRef r m | m -> r where
  {-# MINIMAL new, read, write, (modify_ | modify), (modify_' | modify') #-}

  new :: a -> m (r a)
  read :: r a -> m a
  write :: r a -> a -> m ()

  modify_, modify_' :: r a -> (a -> a) -> m ()
  modify_ r f = void $ modify r f
  modify_' r f = void $ modify' r f

  modify, modify' :: r a -> (a -> a) -> m a
  modify r f = modify_ r f >> read r
  modify' r f = modify_' r f >> read r

instance MonadRef IORef IO where
  new = newIORef
  read = readIORef
  write = writeIORef
  modify_ = modifyIORef 
  modify_' = modifyIORef'

instance MonadRef (STRef s) (ST s) where
  new = newSTRef
  read = readSTRef
  write = writeSTRef 
  modify_ = modifySTRef
  modify_' = modifySTRef'
