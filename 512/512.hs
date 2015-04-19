
import qualified Data.MemoCombinators as Memo

divideEach :: Int -> [(Int, Int)]
divideEach n = a ++ b where
    root = floor $ sqrt $ fromIntegral n
    a = zip (map (\x -> n `div` x) [1 .. root]) $ repeat 1
    k = n `div` (root + 1)
    b = reverse $ map helper [1 .. k] where
        helper k = (k, r - l + 1) where
            l = (n `div` (k + 1)) + 1
            r = n `div` k

bigPhi :: Int -> Int
bigPhi = Memo.bits bigPhi' where
    bigPhi' n = n * (n + 1) `div` 2 - sub where
        sub = sum $ map (\(k, cnt) -> cnt * (bigPhi k)) dpair
        dpair = tail $ divideEach n

f :: Int -> [Int]
f 1 = [1]
f n = ((bigPhi n) - sub) : rec where
    rec = f (n `div` 2)
    sub = sum $ zipWith (*) a rec
    a = 1 : map (*2) a

solve = head $ f 500000000

main = print solve
