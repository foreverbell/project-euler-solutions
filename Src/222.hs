import Control.Monad.ST
import Data.Bits
import Control.Monad
import Common.Array.Unboxed.Vector (unsafeModify)
import qualified Data.Vector.Unboxed.Mutable as V
import Prelude hiding (replicate)

save a b = r1 + r2 - h where
    r1 = fromIntegral a
    r2 = fromIntegral b
    h = sqrt $ (r1 + r2) ^ 2 - (100 - r1 - r2) ^ 2

solve = let u = (2^21) - 1
            update v (-1) = v
            update a b = min a b
            encode i mask = mask * 21 + i
        in runST $ do
            dp <- V.replicate (21 * (u + 1)) (-1) :: ST s (V.MVector s Double)
            forM_ [0 .. 20] $ \i -> V.unsafeWrite dp (encode i (2^i)) $ fromIntegral $ 2 * (i + 30)
            forM_ [1 .. u] $ \mask -> do
                forM_ [0 .. 20] $ \i -> do
                    when (((mask `shiftR` i) .&. 1) == 1) $ do
                        v <- V.unsafeRead dp (encode i mask)
                        forM_ [0 .. 20] $ \j -> do
                            let mask' = mask .|. (1 `shiftL` j)
                            when (mask' /= mask) $ 
                                unsafeModify dp (encode j mask') $ update (v + (fromIntegral ((30 + j) * 2)) - (save (30 + i) (30 + j)))
            return . minimum =<< sequence [ V.unsafeRead dp (encode i u) | i <- [0 .. 20] ]

main = print $ round (1000 * solve)
