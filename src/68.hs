import Data.Array
import Data.List (permutations)
import Common.List (rotate, unique)

solve :: Array Int Int -> [String]
solve permutation = if unique [ sum (pick indexes) | indexes <- gs ] 
    then [showDigit (minimum $ map (concatMap pick) $ map (\r -> rotate r gs) [1 .. 5])]
    else [] where
        gs = [ [1,6,7], [2,7,8], [3,8,9], [4,9,10], [5,10,6] ]
        pick xs = map (\x -> permutation!x) xs
        showDigit xs = concatMap show xs

main = putStrLn $ maximum $ concatMap (\p -> solve (toArray (10:p))) (permutations [1 .. 9]) where
    toArray xs = listArray (1, length xs) xs
