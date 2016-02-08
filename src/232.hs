import qualified Data.MemoCombinators as Memo
import           Text.Printf (printf)

-- the probability that p2 wins when p1 scores a, and p2 scores b (p1 first).
win :: Int -> Int -> Double
win a b = Memo.memo2 Memo.integral Memo.integral win' a b
  where
    win' a b | a >= 100 = 0.0
             | b >= 100 = 1.0
             | otherwise = maximum $ do
                 t1 <- [1 .. 8]
                 t2 <- [1 .. 8]
                 let prob1 = 0.5 ** (fromIntegral t1)
                 let prob2 = 0.5 ** (fromIntegral t2)
                 let d1 = 2 ^ (t1 - 1)
                 let d2 = 2 ^ (t2 - 1)
                 return $ (prob1 / 2 * (win a (b + d1)) + prob2 / 2 * (win (a + 1) (b + d2)) + (1 - prob2) / 2 * (win (a + 1) b)) / (1 - (1 - prob1) / 2)

main = printf "%.8f\n" $ win 0 0
