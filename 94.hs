{-
 - two possible cases:
 - b=a+1: ((3a-1)/2)^2 - 3h^2 = 1
 - b=a-1: ((3a+1)/2)^2 - 3h^2 = 1
 -}

import Common.Util (isqrt)

diophantine d = filter (\(x,y) -> x^2-d*y^2 == 1) convergent where
    root = isqrt d
    cf = helper d root 1 root
    convergent = tail $ zip xs ys
    xs = 1 : root : (zipWith3 (\a b c -> a * b + c) cf (tail xs) xs)
    ys = 0 : 1 : (zipWith3 (\a b c -> a * b + c) cf (tail ys) ys)
    helper n root a b = x : (helper n root a' b') where
        a' = (n - b * b) `div` a
        x = (root + b) `div` a'
        b' = x * a' - b

getCase1 (x, y) = if ((2 * x + 1) `mod` 3 == 0) && ((b * h) `mod` 2 == 0) 
    then a + a + b
    else -1
    where 
        a = (2 * x + 1) `div` 3
        b = a + 1
        h = y

getCase2 (x, y) = if ((2 * x - 1) `mod` 3 == 0) && ((b * h) `mod` 2 == 0)
    then a + a + b
    else -1 
    where 
        a = (2 * x - 1) `div` 3
        b = a - 1
        h = y

main = print $ sum $ filter (\x -> x >= 3 && x <= 10^9) perimeter where
    candidate = takeWhile ((<= 10^9) . fst) $ diophantine 3
    perimeter = [ getCase1 p | p <- candidate ] ++ [ getCase2 p | p <- candidate ]
