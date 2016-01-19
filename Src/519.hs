import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad.ST
import Common.Util (isqrt)
import qualified Common.MonadRef as R

solve :: Int -> Int
solve n = dp ! (n, 1)
  where
    dp = runSTUArray $ do
      dp <- newArray ((0, 0), (n, maxH)) 0 :: ST s (STUArray s (Int, Int) Int)
      writeArray dp (0, 0) 1
      forM_ [1 .. n] $ \i -> do
        forM_ [1 .. min i maxH] $ \j -> do
          v <- R.new 0
          forM_ [0 .. min maxH (j + 1)] $ \k -> do
            let colors = color j k
            ways <- readArray dp (i - j, k)
            R.modify_' v (+ (ways * colors))
            R.modify_' v (`mod` 1000000000)
          writeArray dp (i, j) =<< (R.read v)
      return dp
    color 1 0 = 3
    color _ 0 = 6
    color 1 1 = 2
    color _ 1 = 4
    color _ _ = 1
    maxH = isqrt (2 * n) + 10

main = print $ solve 20000
