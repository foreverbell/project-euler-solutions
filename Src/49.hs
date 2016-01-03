import qualified Data.Set as S
import Data.List (sort)
import Common.Numbers.Primes (primesTo)

main = putStrLn $ concatMap show [a,b,c] where 
    prime4 = dropWhile (< 1000) (primesTo 10000)
    prime4S = S.fromList prime4
    perm = sort . show
    (a,b,c) = head $ [ (a,b,c) 
        | a <- prime4, b <- prime4, 
          a /= 1487, a < b, 
          let c = 2 * b - a, 
          perm a == perm b, perm a == perm c, 
          S.member c prime4S ] 

