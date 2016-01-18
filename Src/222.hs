import           Control.Monad (forM_, when, liftM)
import           Control.Monad.ST (runST, ST)
import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Vector.Unboxed.Mutable as V

save :: Int -> Int -> Double
save a b = r1 + r2 - h 
  where
    r1 = fromIntegral a
    r2 = fromIntegral b
    h = sqrt $ (r1 + r2) ^ 2 - (100 - r1 - r2) ^ 2

solve :: Double
solve = runST $ do
  dp <- V.replicate (21 * (u + 1)) (-1) :: ST s (V.MVector s Double)
  forM_ [0 .. 20] $ \i -> V.unsafeWrite dp (encode i (2^i)) $ fromIntegral $ 2 * (i + 30)
  forM_ [1 .. u] $ \mask -> 
    forM_ [0 .. 20] $ \i ->
      when (((mask `shiftR` i) .&. 1) == 1) $ do
        v <- V.unsafeRead dp (encode i mask)
        forM_ [0 .. 20] $ \j -> do
          let mask' = mask .|. (1 `shiftL` j)
          when (mask' /= mask) $ 
            V.unsafeModify dp (update (v + fromIntegral ((30 + j) * 2) - save (30 + i) (30 + j))) (encode j mask')
  liftM minimum $ sequence [ V.unsafeRead dp (encode i u) | i <- [0 .. 20] ]
  where
    u = (2^21) - 1 :: Int
    update v (-1) = v
    update a b = min a b
    encode i mask = mask * 21 + i

main = print $ round (1000 * solve)
