module Common.Ref (
  Ref (..)
, R
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

modify :: R m => Ref m a -> (a -> a) -> m ()
modify ref f = write ref . f =<< read ref

newRef :: R m => (a -> m ref) -> (ref -> a -> m ()) -> (ref -> m a) -> a -> m (Ref m a)
newRef nw wr rd = liftM (\r -> Ref (wr r) (rd r)) . nw

class Monad m => R m where
  new :: a -> m (Ref m a)

instance R IO where
  new = newRef newIORef writeIORef readIORef

instance R (ST s) where
  new = newRef newSTRef writeSTRef readSTRef
