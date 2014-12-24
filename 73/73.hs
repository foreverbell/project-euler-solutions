
farey (a,b) n
    | r == 0 = (k, n)
    | otherwise = farey (a,b) (n - 1)
    where (k, r) = divMod (1 + a * n) b

farey2 (a,b) (c,d) n = (p,q)
    where
        k = (n + b) `div` d
        p = k * c - a
        q = k * d - b

count a b n = go a (farey a n) b n 0
    where
        go a b f n r
            | b == f = r
            | otherwise = go b next f n $! (r + 1) -- strict
            where next = farey2 a b n

main = print $ count (1,3) (1,2) 12000

-- http://en.wikipedia.org/wiki/Farey_sequence
