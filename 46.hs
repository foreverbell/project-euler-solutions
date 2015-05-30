import qualified Data.Set as S
import Common.Numbers.Primes (testPrime)

findIt :: (S.Set Int) -> Int -> Int
findIt p n = if testPrime n
    then findIt (S.insert n p) (n + 2)
    else if any (\x -> S.member (n - x) p) square2 then findIt p (n + 2) else n
    where square2 = takeWhile (\x -> x < n) [ 2*a*a | a <- [1 .. ] ]

main = print $ findIt (S.fromList [2]) 3
