import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray as MA
import           Data.Array.ST (runSTUArray)
import           Data.Ix (Ix)
import           Control.Monad (forM_)
import           Text.Printf (printf)

import Common.Utils (modifyArray, initArray)

type DPArray = A.UArray (Int, Bool) Double

addArray :: Num e => (MA.MArray a e m, Ix i) => a i e -> i -> e -> m ()
addArray a i d = modifyArray a (+ d) i

go :: DPArray -> (DPArray, Double)
go dp0 = getProbability $ runSTUArray $ do
  dp1 <- MA.newArray ((0, False), (500, True)) 0
  forM_ [0 .. 499] $ \s -> do
    let v = dp0 A.! (s, False)
    addArray dp1 (s, False)     $ v * (fromIntegral s + 1) / 1000
    addArray dp1 (s, True)      $ v * 1 / 1000
    addArray dp1 (s + 1, False) $ v * (998 - 2 * fromIntegral s) / 1000
    let v = dp0 A.! (s, True)
    addArray dp1 (s, True)      $ v * (fromIntegral s + 1) / 1000
    addArray dp1 (s + 1, True)  $ v * (998 - 2 * fromIntegral s) / 1000
  return dp1
  where
    getProbability a = (a, v)
      where v = sum [ a A.! (s, b) | s <- [0 .. 499], b <- [False, True] ]

solve = succ $ sum $ map snd $ take 500 $ iterate (go . fst) (init, 1)
  where
    init :: DPArray
    init = runSTUArray $ initArray ((0, False), (500, True)) 0 [((0, False), 1/1000), ((0, True), 1/1000), ((1, False), 998/1000)]

main = printf "%.8f\n" solve
