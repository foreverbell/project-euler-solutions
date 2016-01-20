import Data.STRef
import Data.Array.ST
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.List (sortBy, groupBy)
import Data.Function (on)

readInput :: IO [[Int]]
readInput = (readFile "input/p082_matrix.txt") >>= (return . (map comma) . words) where
    comma [] = []
    comma (',' : xs) = comma xs
    comma s = (read a) : (comma b)
        where (a, b) = span (/= ',') s

type Vertex = Int
type Edge = (Vertex, Vertex, Int)

extractEdge :: [[Int]] -> (Int, [Edge])
extractEdge mat' = (n * m + 2, concat [ part1, part2, part3 ]) where
    part1 = do
        (x, y) <- [ (x, y) | x <- [0 .. n - 1], y <- [0 .. m - 1] ]
        (dx, dy) <- dirs
        guard $ within (x + dx) (y + dy)
        return $ (label x y, label (x + dx) (y + dy), mat!(x + dx, y + dy))
    part2 = [ (n*m, label i 0, mat!(i,0)) | i <- [0 .. n - 1] ]
    part3 = [ (label i (m-1), n*m+1, 0) | i <- [0 .. n - 1] ]
    n = length mat'
    m = length $ mat'!!0
    mat = listArray ((0, 0), (n - 1, m - 1)) (concat mat') :: Array (Int, Int) Int
    dirs = [(1, 0), (-1, 0), (0, 1)]
    within x y = (x >= 0) && (y >= 0) && (x < n) && (y < m)
    label x y = x * m + y

buildG :: [Edge] -> Int -> Array Vertex [Edge] 
buildG edges n = runSTArray $ do
    arr <- newArray (0, n - 1) [] :: ST s (STArray s Vertex [Edge])
    let groupedEdges = groupBy bar $ sortBy foo edges
    forM_ groupedEdges $ \es -> writeArray arr ((from . head) es) es
    return arr
    where 
        from (u, v, w) = u
        foo = compare `on` from
        bar x y = (from x) == (from y)

dijkstra :: Int -> Int -> Int -> Array Vertex [Edge] -> Int
dijkstra n s t edges = runST $ do
    dist <- newArray (0, n - 1) (maxBound `div` 2) :: ST s (STArray s Int Int)
    vis  <- newArray (0, n - 1) False :: ST s (STArray s Int Bool)
    writeArray dist s 0
    forM_ [1 .. n] $ \_ -> do
        cur <- extractMin n dist vis
        when (cur /= (-1)) $ forM_ (edges!cur) $ \(u, v, w) -> relax dist u v w 
    readArray dist t
    where
        relax dist u v w = do
            old <- readArray dist v
            new <- (readArray dist u) >>= (return . (+w))
            writeArray dist v (min old new)
        extractMin :: Int -> STArray s Int Int -> STArray s Int Bool -> ST s Int
        extractMin n dist vis = do
            best <- newSTRef (-1)
            bestDistance <- newSTRef maxBound
            forM_ [0 .. n - 1] $ \u -> do
                vis' <- readArray vis u
                dist' <- readArray dist u
                temp1 <- readSTRef best
                temp2 <- readSTRef bestDistance
                when ((not vis') && ((temp1 == (-1)) || (dist' < temp2))) $ (writeSTRef best u) >> (writeSTRef bestDistance dist')
            ret <- readSTRef best
            when (ret /= -1) $ writeArray vis ret True
            return ret

main = do
    input <- readInput
    let (n, e) = extractEdge input
    let g = buildG e n
    print $ g `seq` dijkstra n (n - 2) (n - 1) g
