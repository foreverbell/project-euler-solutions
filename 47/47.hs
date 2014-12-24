
numOfDPF x = helper x 2 0  -- O(sqrt x)
    where
        divideAll x p = until (\x -> x `mod` p /= 0) (`div` p) x
        helper x p r
            | r >= 4         = 4  -- optimization
            | x < p * p      = r + 1
            | x `mod` p == 0 = helper (divideAll x p) (p + 1) (r + 1)
            | otherwise      = helper x (p + 1) r

findIt = helper pf 1
    where
        pf = [ numOfDPF x | x <- [1 .. ] ]
        helper xs@(a:b:c:d:rest) n
            | a>=4 && b>=4 && c>=4 && d>=4 = n
            | otherwise                    = helper (tail xs) (n + 1)

main = do
    print findIt

