import qualified Data.MemoCombinators as Memo
import Data.Bits ((.|.))
import Data.Char (toUpper)
import Numeric (showHex)

dp :: Int -> Int -> Bool -> Int
dp = Memo.memo3 Memo.integral Memo.integral Memo.bool dp'
  where
    dp' 0 7 _ = 1
    dp' 0 _ _ = 0
    dp' n bits only0 = sum [ f d | d <- [0 .. 15] ]
      where
        f d = dp (n-1) bits' only0'
          where
            bits' | d == 10 = bits .|. 4
                  | d == 1 = bits .|. 2
                  | d == 0 && not only0 = bits .|. 1
                  | otherwise = bits
            only0' = only0 && (d == 0)

main = putStrLn $ map toUpper $ showHex ret []
  where ret = dp 16 0 True
