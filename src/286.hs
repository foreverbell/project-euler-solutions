import qualified Data.MemoCombinators as Memo
import           Text.Printf (printf)

prob :: Double -> Double
prob q = dp 50 20
  where
    dp n k = Memo.memo2 Memo.integral Memo.integral dp' n k
      where
        dp' 0 0 = 1
        dp' n k | n < k = 0
                | k < 0 = 0
                | otherwise = (dp (n - 1) k) * (1 - hit) + (dp (n - 1) (k - 1)) * hit
          where
            hit = 1 - (fromIntegral n) / q

solve :: Double
solve = helper 50 53 0
  where
    helper l r iter | iter > 1000 = l
                    | v > 0.02 = helper mid r (iter + 1)
                    | otherwise = helper l mid (iter + 1)
      where
        mid = (l + r) / 2
        v = prob mid

main = printf "%.10f\n" solve
