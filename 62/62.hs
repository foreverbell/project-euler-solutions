import Data.List (sort, group, findIndex)

main = print $ index^3 where
    scubes = map (sort . show) [ n^3 | n <- [0 .. 10000] ]
    can' = filter (\x -> length x == 5) (group $ sort scubes)
    can  = map head can' -- unwrap list
    index = (\(Just x) -> x) (findIndex (\x -> x `elem` can) scubes)
