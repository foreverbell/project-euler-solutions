module Common.MonadRef (
  Ref (..)
, MonadRef
, modify
, new
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST)
import Control.Monad (liftM)
import Prelude hiding (read)

data Ref m a = Ref {
  write :: a -> m (),
  read :: m a
}

class Monad m => MonadRef m where
  new :: a -> m (Ref m a)

modify :: MonadRef m => Ref m a -> (a -> a) -> m a
modify ref f = do
  v <- f <$> read ref
  write ref v
  return v

newRef :: MonadRef m => (a -> m ref) -> (ref -> a -> m ()) -> (ref -> m a) -> a -> m (Ref m a)
newRef nw wr rd = liftM (\r -> Ref (wr r) (rd r)) . nw

instance MonadRef IO where
  new = newRef newIORef writeIORef readIORef

instance MonadRef (ST s) where
  new = newRef newSTRef writeSTRef readSTRef
