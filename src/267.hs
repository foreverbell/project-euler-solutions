import Data.Ratio
import Text.Printf (printf)
import qualified Common.Numbers.Numbers as N

binomial :: [Integer]
binomial = [ N.binomial 1000 k | k <- [0 .. 1000] ]

win :: Double -> Int
win f = times 
  where
    base = (1 + 2*f) / (1 - f)
    times = floor $ log' (10**9) - 1000 * log' (1-f) :: Int
    log' x = log x / log base

chance :: Double -> Rational
chance = eval . win
  where eval times = (sum [binomial !! k | k <- [times .. 1000]]) % (2^1000)

findMaximal :: Ord a => (Double -> a) -> Double
findMaximal f = helper 0.0 1.0 0
  where
    helper l r iter | iter > 1000 = l
                    | v1 < v2 = helper l mid2 (iter + 1)
                    | otherwise = helper mid1 r (iter + 1)
      where
        mid1 = (r - l) / 3 + l
        mid2 = (r - l) / 3 * 2 + l
        v1 = f mid1
        v2 = f mid2

main = printf "%.12f\n" (fromRational (chance (findMaximal win)) :: Double)
