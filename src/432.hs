
import Common.Utils (isqrt, if', submasks)
import Common.Numbers.EulerPhi (phiTo)
import Data.List (foldl')
import Data.Bits
import Control.Monad (guard)
import qualified Data.MemoCombinators as Memo
import qualified Data.Vector.Unboxed as V

infixl 6 <+>
(<+>) a b = (a + b) `rem` modulo 
modulo = 10^9 :: Int

n = 10^11 :: Int
n' = truncate $ (fromIntegral n) ** (2 / 3)

bigPhi' = V.fromList $ bigPhi'' where
    phi' = 0 : drop 1 (phiTo n')
    bigPhi'' = 0 : zipWith (<+>) phi' bigPhi''

bigPhi m = if m <= n'
    then bigPhi' V.! m
    else Memo.bits bigPhiMemo m where
        bigPhiMemo m = (f - r1 - r2 + modulo + modulo) `rem` modulo where
            f = (m `rem` modulo) * ((m - 1) `rem` modulo) `quot` 2 `rem` modulo
            root = isqrt m
            r1 = foldl' (<+>) 0 $ map helper [1 .. root] where
                helper r = (bigPhi r) * (m `quot` r - m `quot` (r + 1)) `rem` modulo
            r2 = foldl' (<+>) 0 $ map helper [2 .. m `quot` (root + 1)] where
                helper d = bigPhi (m `quot` d)

dynamic :: Int -> Int -> Int
dynamic n 0 = (bigPhi n) + 1
dynamic n mask = Memo.memo2 Memo.bits (Memo.arrayRange (0, 127)) dynamicMemo n mask where
    dynamicMemo n mask = if' (m == 0) 0 $ (<+>) ((bigPhi m) + 1) $ sum $ do
        sub <- submasks mask
        let cnt = foldl' (<+>) 0 $ do
                sub2 <- submasks mask
                guard $ (sub2 .&. sub) == sub
                return $ (miuMask ((complement sub) .&. sub2)) * (dynamic m sub2)
        let part1 = filterMask (sub .&. mask)
        let part2 = filterMask ((complement sub) .&. mask)
        let multiplier = (product part1) * (product (map pred part2)) - 1
        return $ cnt * multiplier `rem` modulo
        where
            m = n `quot` p
            p = product $ filterMask mask
            miuMask x = if' (odd (popCount x)) (modulo - 1) 1
    filterMask mask = map snd $ filter pred $ zip [0 .. 6] [2, 3, 5, 7, 11, 13, 17] where
        pred (i, x) = (mask .&. (1 `shiftL` i)) /= 0

main = print $ dynamic ((10^11) * 510510) 127
