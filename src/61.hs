
vertices = concatMap process withIndex where
    process (xs, index) = zip (filter check (takeWhile (\x -> x < 10000) xs)) (repeat index) 
    withIndex = zip [triangle, square, pentagonal, hexagonal, heptagonal, octagonal] [1 .. 6]
    triangle = [ n * (n + 1) `div` 2 | n <- [1 .. ] ]
    square = [ n * n | n <- [1 .. ] ]
    pentagonal = [ n * (3 * n - 1) `div` 2 | n <- [1 .. ] ]
    hexagonal = [ n * (2 * n - 1) | n <- [1 .. ] ]
    heptagonal = [ n * (5 * n - 3) `div` 2 | n <- [1 .. ] ] 
    octagonal = [ n * (3 * n - 2) | n <- [1 .. ] ] 
    check x = (length s == 4) && (s!!0 /= '0') && (s!!2 /= '0') where 
        s = show x

main = sequence_ [ dfs [v] 1 | v <- vertices, snd v == 1 ] where
    dfs :: [(Int, Int)] -> Int -> IO ()
    dfs vs 6 = if (t2 $ fst $ head vs) == (f2 $ fst $ last vs) 
        then print $ sum $ map fst vs
        else return ()
    dfs vs size = sequence_ [ dfs (v:vs) (size + 1) | v <- next ] where
        last2 = t2 (fst $ head vs)
        indexes = map snd vs
        next = filter (\(number, index) -> (f2 number == last2 && (index `notElem` indexes))) vertices
    f2 x = take 2 (show x)
    t2 x = drop 2 (show x)

