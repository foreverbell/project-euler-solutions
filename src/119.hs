import Data.List (unfoldr)

sumd n | n < 10 = 0
       | otherwise = sum $ unfoldr f n
  where
    f 0 = Nothing
    f a = Just (a `mod` 10, a `div` 10)

get :: Integer -> [Integer]
get p = map (^p) $ filter (\n -> n == sumd (n^p)) [1 .. 100]

union :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) | x == y = x:union xs ys
                    | x < y  = x:union xs (y:ys)
                    | x > y  = y:union (x:xs) ys

main = print $ (foldr union [] [get p | p <- [2 .. 100]]) !! 29
