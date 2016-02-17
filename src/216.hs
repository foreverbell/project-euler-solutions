import Common.Numbers.Primes (primesTo)
import Common.Numbers.Numbers (tonelliShanks)
import Common.Utils (isqrt)
import Control.Monad (forM_)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

solve :: Int -> Int
solve n = length $ filter id vs
  where
    ps = filter (\p -> p `rem` 8 == 1 || p `rem` 8 == 7) $ primesTo $ isqrt (2*n*n)
    vs = drop 2 $ V.toList $ V.create $ do
      vs <- MV.replicate (n+1) True
      forM_ ps $ \p -> do
        let pick m = min m (p-m)
        let m = pick $ fromJust $ tonelliShanks ((p+1) `quot` 2) p
        let check = if 2*m*m-1 == p then drop 1 else id
        let excludes = check [m, m + p .. n] ++ dropWhile (<=0) [-m, -m + p .. n]
        forM_ excludes $ \i -> do
          MV.write vs i False
      return vs

main = print $ solve 50000000
