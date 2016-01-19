module Common.MonadRef (
  Ref (..)
, MonadRef
, new
, modify
, modify'
) where

import Control.Monad.ST (ST)
import Control.Monad (liftM)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef, modifyIORef')
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef, modifySTRef')

import Prelude hiding (read)

data Ref m a = Ref {
  write :: a -> m ()
, read :: m a
, modify_ :: (a -> a) -> m ()
, modify_' :: (a -> a) -> m ()
}

class Monad m => MonadRef m where
  new :: a -> m (Ref m a)

newRef :: MonadRef m => (a -> m ref) -> (ref -> a -> m ()) -> (ref -> m a) -> (ref -> (a -> a) -> m ()) -> (ref -> (a -> a) -> m ()) -> a -> m (Ref m a)
newRef n w r m m' = liftM (\ref -> Ref (w ref) (r ref) (m ref) (m' ref)) . n

instance MonadRef IO where
  new = newRef newIORef writeIORef readIORef modifyIORef modifyIORef'

instance MonadRef (ST s) where
  new = newRef newSTRef writeSTRef readSTRef modifySTRef modifySTRef'

modify :: MonadRef m => (Ref m a) -> (a -> a) -> m a
modify r f = modify_ r f >> read r

modify' :: MonadRef m => (Ref m a) -> (a -> a) -> m a
modify' r f = modify_' r f >> read r
