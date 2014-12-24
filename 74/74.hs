import Data.Tuple (swap)
import Data.List (unfoldr)
import Data.Array
import qualified Data.Set as S

factorial = 1 : zipWith (*) [1 .. ] factorial

go' x = sum [ factorial !! x | x <- s ]
    where
        s = unfoldr helper x
        helper 0 = Nothing
        helper x = Just $ swap $ divMod x 10

nexts = listArray (0, lim) [ go' x | x <- [0 .. lim] ]
    where lim = 2177280

count x = count' x S.empty 0
    where
        count' :: Int -> S.Set Int -> Int -> Int
        count' x m s
            | s > 60 = s
            | S.member x m = s
            | otherwise = count' next (S.insert x m) (s + 1)
            where next = nexts!x

main = print $ length $ filter (\x -> count x == 60) [1 .. 1000000]
