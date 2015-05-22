
import qualified Data.MemoCombinators as Memo
import Common.List (maximumBy')
import Data.Function (on)

maxN = 1000000

step :: Int -> Int
step 1 = 1
step n = if n > maxN 
    then step' n
    else Memo.arrayRange (1, maxN) step' n where
        step' n = succ $ step $ if odd n 
            then 3 * n + 1
            else n `div` 2

main = print $ maximumBy' (compare `on` step) [1 .. maxN]
