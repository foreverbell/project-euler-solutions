
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isLychrel :: Integer -> Bool
isLychrel x = helper 1 x where
    helper n x
        | n >= 50 = True
        | isPalindrome (show y) = False
        | otherwise = helper (n + 1) y
        where y = x + ((read . reverse . show) x)

main = print $ length $ filter isLychrel [1 .. 9999]
