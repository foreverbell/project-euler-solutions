import Data.Array

primesTo m = eratos [2 .. m] where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p .. m])
    minus (x:xs) (y:ys) = case (compare x y) of 
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys 
        GT -> minus (x:xs) ys
    minus xs _ = xs

isPrime :: Int -> [Int] -> Bool
isPrime x [] = True
isPrime x (p:ps)
    | x < p * p = True
    | x `mod` p == 0 = False
    | otherwise = isPrime x ps

primeList = primesTo 9999 :: [Int]
primeArray = listArray (0, (length primeList) - 1) primeList

canCatArray = listArray (0, (length primeList) - 1) [ catArray (fst p) (snd p) | p <- (zip primeList [0 .. ]) ]
    where
        catNumber x y = read ((show x) ++ (show y)) :: Int
        check x y = (isPrime (catNumber x y) primeList) && (isPrime (catNumber y x) primeList)
        catArray x index = listArray (0, index - 1) [ check x y | y <- take index primeList ]

dfs :: Int -> [Int] -> Int -> Int  -- cur, (p:ps), size, result
dfs _ ps 5 = score ps
    where score = foldl (\s index -> s + primeArray!index) 0
dfs cur ps size = minimum $ (maxBound :: Int) : rec
    where
        canAdd x ps = and [ (canCatArray!index)!x | index <- ps ]
        rec = [ dfs next (next:ps) (size + 1) | next <- [0 .. cur-1], canAdd next ps ]

main = do
    print $ minimum [ dfs i [i] 1 | i <- [0 .. (length primeList) - 1] ]

-- note: this problem is solved by cheating.
-- I just make the assumption that the primes used are less than 10000.
