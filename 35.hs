import Common.Primes (testPrime)
import Common.List (rotate, nub')
import Control.Monad (foldM)

rotateNumber :: Int -> [Int]
rotateNumber x = nub' $ map (\y -> read (rotate y s)) [1 .. length s] where
    s = show x

main = print $ length result where
    primes = filter testPrime $ nub' $ foldM helper 0 $ replicate 6 [1, 3, 7, 9] where
        helper a xs = a : map (\x -> a * 10 + x) xs
    result = 2 : 5 : filter helper primes where
        helper x = all (\y -> y `elem` primes) (rotateNumber x)

