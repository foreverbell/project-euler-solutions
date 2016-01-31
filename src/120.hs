import Common.List (maximumBy')
import Data.Function (on)

rMax :: Int -> Int
rMax a = f $ maximumBy' (compare `on` f) $ take m $ iterate (\(x, y) -> (x * (a - 1) `mod` m, y * (a + 1) `mod` m)) (1, 1) 
  where
    m = a*a
    f (a, b) = (a + b) `mod` m

main = print $ sum $ map rMax [3 .. 1000]
