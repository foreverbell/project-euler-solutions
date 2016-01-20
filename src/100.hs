
possiblePairs :: [(Integer, Integer)]
possiblePairs = (1,1) : map foo possiblePairs where
    foo (a,b) = (3*a+4*b-3, 2*a+3*b-2)

main = print $ snd $ head $ dropWhile ((<= bound) . fst) $ possiblePairs where
    bound = 10^12
