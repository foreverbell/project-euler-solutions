import Data.List

main = do
    print $ (sort (permutations ['0' .. '9'])) !! 999999
