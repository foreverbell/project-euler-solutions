import qualified Data.Set as S

isPrime :: Int -> Bool
isPrime x
    | x <= 0    = False
    | x == 1    = False
    | otherwise = all (\y -> x `mod` y /= 0) [2 .. (floor $ sqrt $ fromIntegral x)]

findIt :: (S.Set Int) -> Int -> Int
findIt p n
    | isPrime n = findIt (S.insert n p) (n + 2)
    | otherwise = 
        if any (\x -> S.member (n - x) p) square2 then
            findIt p (n + 2)
        else
            n
    where 
        square2 = takeWhile (\x -> x < n) [ 2*a*a | a <- [1 .. ] ]

main = do
    print $ findIt (S.fromList [2]) 3
