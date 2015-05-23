import Data.Array.IArray

modulo = 1000000 :: Int

ways :: Array Int Int
ways = listArray (0, 100000) [ dp n | n <- [0 .. 100000] ] where
    dp 0 = 1
    dp n = helper 1 1 where
        helper j r = if val1 >= 0
            then (acc + (helper (j + 1) (-r))) `mod` modulo
            else 0 where
                val1 = n - ((3 * j * j - j) `div` 2)
                val2 = n - ((3 * j * j + j) `div` 2)
                acc1 = r * (ways ! val1)
                acc2 = if (val2 >= 0) 
                    then r * (ways ! val2)
                    else 0
                acc = acc1 + acc2

find0 ((index, 0) : _) = index
find0 (_ : xs) = find0 xs

main = print $ find0 $ assocs ways

-- http://en.wikipedia.org/wiki/Pentagonal_number_theorem

