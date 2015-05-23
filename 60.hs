import Data.Array
import Common.Primes (primesTo, testPrime)

primeList = primesTo 9999 :: [Int]
primeArray = listArray (0, (length primeList) - 1) primeList

canCatArray = listArray (0, (length primeList) - 1) [ catArray (fst p) (snd p) | p <- (zip primeList [0 .. ]) ] where
    check x y = (testPrime (cat x y)) && (testPrime (cat y x)) where
        cat x y = read $ (show x) ++ (show y)
    catArray x index = listArray (0, index - 1) [ check x y | y <- take index primeList ]

main = print $ minimum [ dfs i [i] 1 | i <- [0 .. (length primeList) - 1] ] where
    dfs :: Int -> [Int] -> Int -> Int  -- cur, (p:ps), size, result
    dfs _ ps 5 = score ps where 
        score = foldl (\s index -> s + primeArray!index) 0
    dfs cur ps size = minimum $ maxBound : rec where
        canAdd x ps = and [ (canCatArray!index)!x | index <- ps ]
        rec = [ dfs next (next:ps) (size + 1) | next <- [0 .. cur-1], canAdd next ps ]

