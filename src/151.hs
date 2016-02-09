{-# LANGUAGE Rank2Types #-}

import qualified Data.MemoCombinators as Memo
import           Text.Printf (printf)

memo4 :: Memo.Memo a -> Memo.Memo b -> Memo.Memo c -> Memo.Memo d -> (a -> b -> c -> d -> r) -> (a -> b -> c -> d -> r)
memo4 a b c d = a . (Memo.memo3 b c d .)

solve :: Int -> Int -> Int -> Int -> Double
solve a2 a3 a4 a5 = memo4 Memo.integral Memo.integral Memo.integral Memo.integral solve' a2 a3 a4 a5
  where
    solve' 0 0 0 1 = 0
    solve' a2 a3 a4 a5 = v1 + v2 + v3 + v4 + v5
      where
        s = a2 + a3 + a4 + a5
        p2 = fromIntegral a2 / fromIntegral s
        p3 = fromIntegral a3 / fromIntegral s
        p4 = fromIntegral a4 / fromIntegral s
        p5 = fromIntegral a5 / fromIntegral s
        v1 = if s==1 then 1 else 0
        v2 = if a2>0 then p2 * solve (a2-1) (a3+1) (a4+1) (a5+1) else 0
        v3 = if a3>0 then p3 * solve a2 (a3-1) (a4+1) (a5+1) else 0
        v4 = if a4>0 then p4 * solve a2 a3 (a4-1) (a5+1) else 0
        v5 = if a5>0 then p5 * solve a2 a3 a4 (a5-1) else 0

main = printf "%.6f\n" $ solve 1 1 1 1
