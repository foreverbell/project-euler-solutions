
import Control.Applicative
import Data.Function (on)
import Data.List (foldl1')

limit = 2000000
item = takeWhile (\(x,y) -> y < limit) [ (n, (1 + n) * n `div` 2) | n <- [1 .. ] ]

near (x,y) = abs (y - limit)

minimumBy' cmp xs = foldl1' helper xs
    where
        helper x y = case cmp x y of
            GT -> y
            _  -> x

main = print $ fst $ minimumBy' (compare `on` near) $ comb <$> item <*> item
    where comb (a,b) (c,d) = (a*c, b*d)

