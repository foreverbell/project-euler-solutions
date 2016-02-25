import Data.List (nub, sort)
import System.Random (mkStdGen)
import Common.Numbers.Primes (factorize)

rad n = product $ nub ds
  where ds = fst $ factorize (mkStdGen n) n

main = print $ snd $ sort [ (rad n, n) | n <- [1 .. 100000] ] !! 9999
