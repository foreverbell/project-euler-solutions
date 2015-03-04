
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List (nub)

maxK = 12000

solve :: Array Int Int
solve = runSTArray $ do
    ret <- newArray (1, maxK) maxBound :: ST s (STArray s Int Int)
    dfs ret 0 1 0 2
    return ret
    where
        update ret idx val = do
            old <- readArray ret idx
            writeArray ret idx (min val old)
        dfs ret count product sum last = do
            forM_ [last .. maxK] $ \d -> do
                let product' = product * d
                let sum' = sum + d
                let count' = count + 1
                let k' = product' - sum' + count'
                if (k' <= maxK) 
                    then do
                        update ret k' product'
                        dfs ret count' product' sum' d
                    else return ()

main = print $ (sum . nub . drop 1 . elems) solve
