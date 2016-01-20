import Data.List (sort, group, findIndex)
import Data.Maybe (fromJust)

main = print $ index^3 where
    scubes = map (sort . show) [ n^3 | n <- [0 .. 10000] ]
    can  = map head $ filter (\x -> length x == 5) (group $ sort scubes)
    index = fromJust (findIndex (\x -> x `elem` can) scubes)
