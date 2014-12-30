
import Data.Graph
import Data.List
import Data.Char (digitToInt)

solve :: [String] -> String
solve logs' = intersect (concatMap show result) (concat logs')
    where
        md2i = map digitToInt
        logs = map md2i logs'
        buildEdges [x,y,z] = [(x,y), (y,z)]
        edges = concatMap buildEdges logs
        g = buildG (0,9) edges
        result = topSort g

main = (readFile "p079_keylog.txt") >>= (putStrLn . solve . lines)
    
