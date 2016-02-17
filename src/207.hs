import Data.Bits ((.&.))
import Data.List (scanl')

proportions = zip [1 .. ] $ scanl' f (0, 0) [2 .. ]
  where
    isPower2 :: Int -> Bool
    isPower2 x = x .&. (-x) == x
    f (a, b) i | isPower2 i = (a + 1, b + 1)
               | otherwise = (a, b + 1)

solve = n*n - n
  where
    f (_, (a, b)) = 12345 * a >= b 
    n = fst $ head $ dropWhile f proportions

main = print (solve :: Int)
