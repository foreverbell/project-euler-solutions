import Data.List

isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal [_] = True
isPal (x:xs) 
    | x == (last xs) = isPal (init xs)
    | otherwise = False

main = print (last (filter (isPal . show) (sort (foldl' (\xs x -> xs ++ (map (* x) [100 .. 999])) [] [100 .. 999]))))
