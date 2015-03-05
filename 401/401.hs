import Data.List (foldl')

sumModulo :: Int -> [Int] -> Int
sumModulo m xs = foldl' helper 0 xs where
    helper accum x = (accum + (x `mod` m)) `mod` m

sumSquared :: Int -> Int -> Int -> Int
sumSquared m l r = case compare l r of
    GT -> 0
    _ -> ((sumSquared' m r) - (sumSquared' m (l - 1))) `mod` m 
    where 
        sumSquared' m n = fromInteger (((n' * (1 + n') * (1 + 2 * n')) `div` 6) `mod` m') where
            n' = toInteger n
            m' = toInteger m

sigma2 :: Int -> Int -> Int
sigma2 m n = (part1 + part2) `mod` m where 
    root = (floor . sqrt . fromIntegral) n
    upper = n `div` (root + 1)
    part1 = sumModulo m [ (((((n `div` i) `mod` m) * i) `mod` m) * i) `mod` m | i <- [1 .. upper] ]
    part2 = sumModulo m $ do
        k <- [1 .. root]
        let lo = n `div` (k + 1) + 1
        let hi = n `div` k
        return $ (k * (sumSquared m lo hi)) `mod` m
    
main = print $ sigma2 (10^9) (10^15)
