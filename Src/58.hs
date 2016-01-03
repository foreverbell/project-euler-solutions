
import Common.Numbers.Primes (testPrime)

solve :: [Int] -> [Int] -> [Int] -> Int -> Int -> Int
solve seq1 seq2 seq3 p dep = if ptotal * 10 < total
    then dep * 2 - 1
    else solve (tail seq1) (tail seq2) (tail seq3) ptotal (dep + 1)
    where 
        wrap' True = 1
        wrap' False = 0
        wrap = wrap' . testPrime . head
        total = 4 * dep - 3
        ptotal = p + (wrap seq1) + (wrap seq2) + (wrap seq3)

main = print $ solve seq1 seq2 seq3 0 2 where
    seq1 = [ 4*n^2-10*n+7 | n <- [2 .. ] ]
    seq2 = [ 4*n^2+1      | n <- [1 .. ] ]
    seq3 = [ 4*n^2-6*n+3  | n <- [2 .. ] ]

