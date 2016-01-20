import Data.Array.ST
import Control.Monad
import Control.Monad.ST

getShortest :: [[Int]] -> Int
getShortest mat = runST $ do
    dp <- newArray ((0, 0), (n - 1, m - 1)) 0 :: ST s (STArray s (Int, Int) Int)
    forM_ [0 .. n - 1] $ \i -> do
        forM_ [0 .. m - 1] $ \j -> do
            update dp i j
    readArray dp (n - 1, m - 1)
    where
        n = length mat
        m = length $ mat!!0
        update :: STArray s (Int, Int) Int -> Int -> Int -> ST s ()
        update dp 0 0 = writeArray dp (0, 0) ((mat!!0)!!0)
        update dp 0 j = do
            go <- readArray dp (0, j - 1) 
            writeArray dp (0, j) (go + ((mat!!0)!!j))
        update dp i 0 = do
            go <- readArray dp (i - 1, 0)
            writeArray dp (i, 0) (go + ((mat!!i)!!0))
        update dp i j = do
            go1 <- readArray dp (i - 1, j)
            go2 <- readArray dp (i, j - 1)
            let go = min go1 go2
            writeArray dp (i, j) (go + ((mat!!i)!!j))

comma :: String -> [Int]
comma [] = []
comma (',' : xs) = comma xs
comma s = (read a) : (comma b)
    where (a, b) = span (/= ',') s

readInput :: IO [[Int]]
readInput = (readFile "input/p081_matrix.txt") >>= (return . (map comma) . words)

main = readInput >>= (print . getShortest)
