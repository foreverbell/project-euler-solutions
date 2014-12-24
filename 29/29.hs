import Data.List

main = do
    print $ length term
    where 
        term = nub [ a^b | a <- [2 .. 100], b <- [2 .. 100] ]
