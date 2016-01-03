import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import qualified Common.Ref as R
import qualified Common.DataStructure.UnionFind as UF
import Data.List (sortBy)
import Data.Function (on)

kruskal :: Int -> [(Int, Int, Int)] -> Int
kruskal n g = runST $ do
    acc <- R.new 0
    ufs <- UF.make (0, n - 1) :: ST s (UF.UFSet (STUArray s) (ST s))
    let sortedG = sortBy (compare `on` weight) g
    forM_ sortedG $ \(u, v, w) -> do
        merged <- UF.union ufs u v
        when merged $ do
            R.modify acc ((+) w)
    R.read acc
    where weight (u, v, w) = w

comma :: String -> [String]
comma [] = []
comma (',' : xs) = comma xs
comma s = a : (comma b)
    where (a, b) = span (/= ',') s

readInput :: IO (Int, Int, [(Int, Int, Int)])
readInput = do
    dat <- readFile "Input/p107_network.txt"
    let g = concat $ parse dat
    let n = length $ words dat
    let s = sum $ map (\(u, v, w) -> w) g
    return (n, s, g) where
        parse dat = map parse' $ zip (words dat) [0 .. ]
        parse' (dat, u) = do
            (w, v) <- filter (\(_, v) -> u < v) $ filter ((/= "-") . fst) $ zip (comma dat) [0 .. ]
            return (u, v, read w)

main = do
    (n, s, g) <- readInput
    print $ s - (kruskal n g)
