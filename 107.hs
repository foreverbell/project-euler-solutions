import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List (sortBy)
import Data.Function (on)

kruskal :: Int -> [(Int, Int, Int)] -> Int
kruskal n g = runST $ do
    acc <- newSTRef 0
    ufSet <- newArray (0, n - 1) 0 :: ST s (STArray s Int Int)
    initSet ufSet n
    let sortedG = sortBy (compare `on` weight) g
    forM_ sortedG $ \(u, v, w) -> do
        eq <- unionSet ufSet u v
        when eq $ do
            acc' <- readSTRef acc
            writeSTRef acc (acc' + w)
    readSTRef acc
    where
        weight (u, v, w) = w
        initSet ufSet n = do
            forM_ [0 .. n - 1] $ \i -> do
            writeArray ufSet i i
        findSet ufSet u = do
            v <- readArray ufSet u
            if (u == v) 
                then return u 
                else do
                    v' <- findSet ufSet v
                    writeArray ufSet u v'
                    return v'
        unionSet ufSet u v = do
            u' <- findSet ufSet u
            v' <- findSet ufSet v
            if (u' == v')
                then return False
                else do
                    writeArray ufSet u' v'
                    return True

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
