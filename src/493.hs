
import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Text.Printf

comb n m = (product [m + 1 .. n]) `div` (product [1 .. n - m])

dynamic :: Int -> Array (Int, Int) Int -> Double
dynamic 8 dp = a / b where
    count = [ dp!(20,k) | k <- [1 .. 7] ]
    a = fromIntegral $ sum $ zipWith (*) count [1 .. 7]
    b = fromIntegral $ sum count
dynamic n dp = dynamic (n + 1) $ runSTArray $ do
    ret <- thaw dp
    forM_ [0 .. 20] $ \m -> do
        forM_ [0 .. n] $ \k -> do
            when (k /= 0) $ do
                forM_ (take 10 [1 .. m]) $ \i -> do
                    let a = dp!(m - i, k - 1)
                    b <- readArray ret (m, k)
                    writeArray ret (m, k) (b + a * (comb 10 i))
    return ret

solve = dynamic 1 initArray where
    initArray = runSTArray $ do
        ret <- newArray ((0, 0), (20, 7)) 0
        writeArray ret (0, 0) 1
        return ret

main = putStrLn $ printf "%.9f" solve
