
import Control.Monad (guard)

solve n = n + 2 * k where
    p3 = 20 : map findNext p3
    k = last $ takeWhile (<= n) p3
    findNext n = n + k where
        factorize n = concat $ do
            let root = (floor . sqrt . fromIntegral) n 
            d <- [1 .. root]
            guard $ (n `mod` d) == 0
            if (d * d == n)
                then return [d]
                else return [d, n `div` d]
        factors = drop 1 $ factorize $ (2 * n - 1)
        k = minimum [((n `div` d) + 1) * d - n | d <- factors]

main = print $ solve (10^15)
