
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (guard, forM_, when)
import Control.Monad.ST
import Text.Printf

convert = undefined

dynamic :: Int -> Int -> [(Int, Int)]
dynamic times dice = assocs $ rec times dice init where
    cnt = times * dice
    init = listArray (0, cnt) (1 : (repeat 0))
    rec :: Int -> Int -> UArray Int Int -> UArray Int Int
    rec 0 _ ret = ret
    rec times dice last = rec (times - 1) dice $ runSTUArray $ do
        ret <- newArray (0, cnt) 0
        forM_ [cnt, cnt - 1 .. 1] $ \i -> do
            forM_ [1 .. dice] $ \j -> do
                when (i >= j) $ do
                    v <- readArray ret i
                    writeArray ret i (v + last!(i - j))
        return ret

peter = dynamic 9 4
colin = dynamic 6 6 

solve = sum $ do
    (a, b) <- peter
    (c, d) <- colin
    guard $ a > c
    return $ b * d

main = putStrLn $ printf "%.7f" (a / b) where
    a = fromIntegral solve :: Double
    b = (4^9)*(6^6) :: Double
