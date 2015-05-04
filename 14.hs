import Data.Array
import Data.List (maximumBy)
import Data.Function (on)

-- stores the answer from 1 to 1000000
main = print $ maximumBy (compare `on` (\n -> r!n)) [1 .. lim]
    where
        lim = 1000000
        r = listArray (1,lim) (map step [1 .. lim])
        step 1 = 1
        step x
            | odd x    = 1 + (step' $ 3 * x + 1)
            | even x   = 1 + (step' $ x `div` 2)
        step' x
            | x <= lim = r!x
            | otherwise = step x
