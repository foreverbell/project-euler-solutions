import Control.Monad (forM_, when)
import Control.Monad.ST
import Data.Array.ST
import Data.Array ((!))
import Data.Bits ((.|.))

readInput :: IO [[Int]]
readInput = readFile "Input/p345_input.txt" >>= process
  where process d = return $ map (map read . words) (lines d)

solve :: [[Int]] -> Int
solve input = runST $ do
  let n = length input
  matrix0 <- newArray ((0, 0), (n - 1, n - 1)) 0 :: ST s (STArray s (Int, Int) Int)
  dp <- newArray ((0, 0), (n - 1, 2^n-1)) (-1) :: ST s (STArray s (Int, Int) Int)
  forM_ [0 .. n - 1] $ \i -> 
    forM_ [0 .. n - 1] $ \j -> 
      writeArray matrix0 (i, j) $ (input !! i) !! j
  matrix <- freeze matrix0
  forM_ [0 .. n - 1] $ \i -> writeArray dp (0, 2^i) $ matrix ! (0, i)
  forM_ [0 .. n - 2] $ \i -> 
    forM_ [0 .. (2^n) - 1] $ \mask -> do
      val <- readArray dp (i, mask)
      when (val /= (-1)) $ 
        forM_ [0 .. n - 1] $ \j -> do
          let mask0 = mask .|. (2^j)
          when (mask0 /= mask) $ do
            let val0 = val + matrix ! (i + 1, j)
            old <- readArray dp (i + 1, mask0)
            writeArray dp (i + 1, mask0) (max old val0)
  readArray dp (n - 1, 2^n-1)

main = readInput >>= (print . solve)
