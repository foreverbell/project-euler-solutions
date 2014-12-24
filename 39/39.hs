import Data.Function (on)
import Data.List (maximumBy)

rightTriple p = [ (a,b,c) | a <- [1 .. p `div` 3], b <- [a .. ((p - a) `div` 2)], let c = p - a - b, a * a + b * b == c * c]

main = do
    print $ maximumBy (compare `on` length . rightTriple) [1 .. 1000]
