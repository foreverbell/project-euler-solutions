import Control.Monad
import Control.Monad.ST
import qualified Common.MonadRef as R
import qualified Common.DataStructure.UnionFind as UF
import Data.List (sortBy)
import Data.Function (on)

type Edge = (Int, Int, Int)
type Graph = (Int, Int, [Edge]) -- n, sum of edges, edges

weight :: Edge -> Int
weight (_, _, w) = w

kruskal :: Graph -> Int
kruskal (n, _, edges) = runST $ do
  acc <- R.new 0
  ufs <- UF.make n
  let sortedE = sortBy (compare `on` weight) edges
  forM_ sortedE $ \(u, v, w) -> do
    merged <- UF.union ufs u v
    when merged $ R.modify_' acc (+ w)
  R.read acc

comma :: String -> [String]
comma [] = []
comma (',':xs) = comma xs
comma s = a:comma b
  where 
    (a, b) = span (/= ',') s

readInput :: IO Graph
readInput = do
  input <- readFile "input/p107_network.txt"
  let e = concat $ parse input
  let n = length $ words input
  let s = sum $ map weight e
  return (n, s, e) 
  where
    parse input = zipWith f (words input) [0 .. ]
      where
        f input u = do
          (w, v) <- filter (\(_, v) -> u < v) $ filter ((/= "-") . fst) $ zip (comma input) [0 .. ]
          return (u, v, read w)

main = do
  g@(_, s, _) <- readInput
  print $ s - kruskal g
