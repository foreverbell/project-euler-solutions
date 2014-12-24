import Data.Array
import Data.List (group, permutations)

rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

solve :: Array Int Int -> [String]
solve dat
    | length (group sums) == 1 = [sdigit (minimum $ generate 5 belong [])]
    | otherwise = []
    where
        belong = [ [1,6,7], [2,7,8], [3,8,9], [4,9,10], [5,10,6] ]
        sums = [ helper indexes | indexes <- belong ] :: [Int]
            where helper indexes = sum [ dat!i | i <- indexes ]
        toVal belong = concatMap helper belong
            where helper indexes = [ dat!i | i <- indexes ]
        sdigit xs = concatMap show xs
        generate 0 _ xs = xs
        generate dep cur xs = generate (dep - 1) (rotate cur) ((toVal cur) : xs)

main = putStrLn $ maximum $ concatMap (\p -> solve (toArray (10:p))) (permutations [1 .. 9])
    where
        toArray xs = listArray (1, length xs) xs
