
splitByComma :: String -> [Int]
splitByComma [] = []
splitByComma (',' : xs) = splitByComma xs
splitByComma s = (read a) : (splitByComma b)
    where (a, b) = span (/= ',') s

readInput :: IO [[Int]]
readInput = readFile "input/p102_triangles.txt" >>= (return . map splitByComma . words)

crossSgn :: (Int, Int, Int, Int) -> Int
crossSgn (x1, y1, x2, y2) = case compare cp 0 of
    GT -> 1
    EQ -> 0
    LT -> -1
    where cp = (x1 - x2) * y2 - (y1 - y2) * x2

solveSingle :: [Int] -> Bool
solveSingle [a,b,c,d,e,f] = (length $ filter (> 0) sgn) * (length $ filter (< 0) sgn) == 0
    where
        sgn = [crossSgn (a,b,c,d), crossSgn (c,d,e,f), crossSgn (e,f,a,b)]

main = readInput >>= ((print . length . filter id) . (map solveSingle))

