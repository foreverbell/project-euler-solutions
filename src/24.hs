import Data.List (permutations, sort)

main = putStrLn $ (sort (permutations ['0' .. '9'])) !! 999999
