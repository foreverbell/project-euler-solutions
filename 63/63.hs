
count base = length $ filter (\(a, b) -> nLength a == b) $ takeWhile (\(a, b) -> nLength a >= b) can
    where 
        can = [ (base^i, i) | i <- [1 .. ] ]
        nLength = length . show

main = do
    print $ 1 + sum [ count i | i <- [2 .. 9] ]
