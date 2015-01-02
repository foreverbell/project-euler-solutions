import Data.List (permutations, sort)

main = print $ (sort (permutations ['0' .. '9'])) !! 999999
