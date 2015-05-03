
import Control.Monad (guard)
import Data.Array.Unboxed
import Text.Printf (printf)
import Control.Parallel.Strategies (parMap, rdeepseq, using, Strategy, NFData)
import Control.Parallel (pseq)

countBelow n (x1, y1) (x2, y2) = sum [ count x | x <- [0 .. n] ] where
    count x = (clamp y (-1) n) + 1 where
        y = (y2 - y1) * (x - x1) `div` (x2 - x1) + y1
        clamp y a b = min (max y a) b

countOn n (x1, y1) (x2, y2) strict = if strict 
    then count (min x1 x2) (max x1 x2)
    else count 0 n
    where
        d = gcd (x2 - x1) (y2 - y1)
        dx = (x2 - x1) `div` d
        dy = (y2 - y1) `div` d
        count a b = (helper [(x1 + i * dx, y1 + i * dy) | i <- [0 .. ]]) + (helper [(x1 - i * dx, y1 - i * dy) | i <- [1 .. ]]) where
            helper  = length . (takeWhile (\(x, y) -> x >= a && x <= b && y >= 0 && y <= n))

countExcludes n p1@(x1, y1) p2@(x2, y2) = if (x1 == x2)
    then countExcludes n (y2, x2) (y1, x1)
    else if x1 < x2
        then below - on
        else (n + 1) * (n + 1) - below + (on' - on)
    where
        below = countBelow n p1 p2
        on = countOn n p1 p2 True
        on' = countOn n p1 p2 False

mapReduce :: (NFData b, NFData c) => Int -> (a -> b) -> ([b] -> c) -> [a] -> c
mapReduce n mapFunc reduceFunc xs = mapResult `pseq` reduceResult where
    mapResult = concat $ parMap rdeepseq (map mapFunc) (chunk n xs)
    reduceResult = reduceFunc mapResult `using` rdeepseq
    chunk _ [] = []
    chunk n xs = as : chunk n bs where (as, bs) = splitAt n xs

solve :: Int -> Double
solve n = mapReduce n solve sum pts where
    pts = [ (i, j) | i <- [0 .. n], j <- [0 .. n] ] :: [(Int, Int)]
    prob = 1.0 / (fromIntegral (n + 1)) :: Double
    probPowers' = 1 : map (\x -> x * (1 - prob)) probPowers'
    probPowers = listArray (0, (n + 1) * (n + 1)) probPowers' :: Array Int Double
    solve p1 = sum $ [solve' (p1, p2) | p2 <- pts, p1 /= p2]
    solve' (p1, p2) = prob * prob * (probPowers!excluded) * area where
        excluded = countExcludes n p1 p2
        area = (fromIntegral ((fst p1) * (snd p2) - (fst p2) * (snd p1))) / 2

main = printf "%.5f\n" (solve 100)

