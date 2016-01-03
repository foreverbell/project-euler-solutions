
isqrt = floor . sqrt . fromIntegral

isDesired :: Int -> Bool
isDesired x = check (show (x * x)) '1' where
    check (x:[]) n = True
    check (x:xs) n = if (x == n) then check (tail xs) (succ x) else False

solve = head $ filter isDesired [lo, lo + 10 .. ] where
    lo = isqrt (10^18)

main = print $ solve
