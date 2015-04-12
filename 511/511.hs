
import Control.Monad (filterM, forM_)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

m = 10^9
k = 4321
n = 1234567898765
pd = [5, 41, 25343, 237631]
divisor = map product xs where
    xs = filterM (const [False, True]) pd

conv :: V.Vector Int -> V.Vector Int -> V.Vector Int
conv a b = V.create $ do
    ret <- MV.replicate k 0
    forM_ [0 .. k - 1] $ \i -> do
        forM_ [0 .. k - 1] $ \j -> do
            let k' = (i + j) `mod` k
            let v = ((a!i) * (b!j)) `mod` m
            v' <- MV.read ret k'
            MV.write ret k' $ (v + v') `mod` m
    return ret

power :: V.Vector Int -> Int -> V.Vector Int
power a p = helper a p init where
    init = V.fromListN k (1 : repeat 0)
    helper a 0 ret = ret `seq` ret
    helper a p ret = if odd p
        then helper a' p' (conv ret a)
        else helper a' p' ret where
            a' = conv a a
            p' = p `div` 2

solve = poly ! (n `mod` k) where
    d = map (\x -> k - (x `mod` k)) divisor
    base = V.fromList $ map (\x -> count x d) [0 .. k - 1] where
        count x xs = length $ filter (== x) xs
    poly = power base n

main = print solve
