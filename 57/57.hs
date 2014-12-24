-- a/b -> (a+2b)/(a+b)

main = do
    print $ length $ filter (\(a,b) -> length (show a) > length (show b)) (take 1000 sqrt2)
    where sqrt2 = (3,2) : map (\(a,b) -> (a+2*b,a+b)) sqrt2
