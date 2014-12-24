import Data.Array.IArray

type IntArray = Array Int Int

modulo = 1000000 :: Int

mod' x m
    | y < 0 = y + m
    | otherwise = y
    where y = x `mod` m

ways :: IntArray
ways = listArray (0, 100000) [ dp n | n <- [0 .. 100000] ] where
    dp 0 = 1
    dp n = helper 1 1 where
        helper j r
            | val1 >= 0 = (acc + (helper (j + 1) (-r))) `mod'` modulo
            | otherwise = 0
            where
                val1 = n - ((3 * j * j - j) `div` 2)
                val2 = n - ((3 * j * j + j) `div` 2)
                acc1 = r * (ways ! val1)
                acc2
                    | val2 >= 0 = r * (ways ! val2)
                    | otherwise = 0
                acc = acc1 + acc2

find0 ((index, x) : xs)
    | x == 0 = index
    | otherwise = find0 xs

main = print $ find0 $ assocs ways

-- http://en.wikipedia.org/wiki/Pentagonal_number_theorem

