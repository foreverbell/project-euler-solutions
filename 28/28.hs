solve n = 4 * part1 - 6 * part2 - 3
    where
        part1 = sum [x^2 | x <- [1, 3 .. n]]
        part2 = sum [2, 4 .. n - 1]

main = do
    print $ solve 1001
