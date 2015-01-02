
isPrime :: Int -> Bool
isPrime x
    | x == 1 = False
    | otherwise = all (\y -> x `mod` y /= 0) (2 : [3, 5 .. (floor $ sqrt $ fromIntegral x)])

solve :: [Int] -> [Int] -> [Int] -> Int -> Int -> Int
solve seq1 seq2 seq3 p dep
    | ptotal * 10 < total = dep * 2 - 1
    | otherwise = solve (tail seq1) (tail seq2) (tail seq3) ptotal (dep + 1)
    where 
        wrap' True = 1
        wrap' False = 0
        wrap = wrap' . isPrime . head
        total = 4 * dep - 3
        ptotal = p + (wrap seq1) + (wrap seq2) + (wrap seq3)

main = print $ solve seq1 seq2 seq3 0 2 where
    seq1 = [ 4*n^2-10*n+7 | n <- [2 .. ] ]
    seq2 = [ 4*n^2+1      | n <- [1 .. ] ]
    seq3 = [ 4*n^2-6*n+3  | n <- [2 .. ] ]

