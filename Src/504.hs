
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_, guard)
import Control.Monad.ST

isSquared x = root * root == x where
    root = (floor . sqrt . fromIntegral) x

lattices m = runSTUArray $ do
    ret <- newArray ((1, 1), (m, m)) 0 :: ST s (STUArray s (Int, Int) Int)
    forM_ [1 .. m] $ \i -> do
        forM_ [1 .. m] $ \j -> do
            writeArray ret (i, j) (count i j)
    return ret where
        count a b = sum $ map get [0 .. a] where
            get x = (y `div` a - (if (y `mod` a == 0) then 1 else 0)) + 1 where
                y = b * x

solve m = ret where
    lat = lattices m
    ret = length $ do
        a <- [1 .. m]
        b <- [1 .. m]
        c <- [1 .. m]
        d <- [1 .. m]
        let nLattices = lat!(a,b) + lat!(c,b) + lat!(a,d) + lat!(c,d) - a - b - c - d + 1
        guard $ isSquared nLattices
        return nLattices

main = print $ solve 100
