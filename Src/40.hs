import Data.Char

main = print $ product $ map digitToInt [ numberSeq !! i | i <- [1, 10, 100, 1000, 10000, 100000, 1000000] ]
    where numberSeq = '0' : (take 1000000 $ concatMap show [1 .. ])
