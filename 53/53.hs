
pascal :: [[Integer]]
pascal = [1] : map (\xs -> zipWith (+) (0 : xs) (xs ++ [0])) pascal

main = do
    print $ length $ filter (>1000000) l
    where
        l = concat $ take 101 pascal
