
import Data.List (findIndex, sortBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Array
import Data.Array.ST
import Control.Monad (forM_)
import Control.Monad.ST
import Text.Printf

squares = ["GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2"]
nDice = 4
nSquare = length squares

getIndex :: String -> Int
getIndex square = fromJust $ findIndex (== square) squares

fall :: String -> [String]
fall "G2J" = ["JAIL"]
fall cc@('C':'C':c) = "GO" : "JAIL" : replicate 14 cc 
fall ch@('C':'H':c) = "GO" : "JAIL" : "C1" : "E3" : "H2" : "R1" : nextR : nextR : nextU : back3 : replicate 6 ch where
    index = getIndex ch
    back3 = squares !! ((index - 3) `mod` nSquare)
    nextR = case c of
        "1" -> "R2"
        "2" -> "R3"
        "3" -> "R1"
    nextU = case c of
        "1" -> "U1"
        "2" -> "U2"
        "3" -> "U1"
fall x = [x]

type State = (Int, Int) -- (sqr, doubles)
type StateArray = Array State Double

go' :: State -> Double -> [(State, Double)]
go' (sqr, doubles) prob = concat $ do
    x <- [1 .. nDice]
    y <- [1 .. nDice]
    let doubles' = if x == y then (doubles + 1) else 0
    let sqr' = squares !! ((sqr + x + y) `mod` nSquare)
    let (sqr'', doubles'') = tryGoJail (sqr', doubles')
    let falls = fall sqr''
    let prob'' = prob / ((fromIntegral nDice) ^ 2) / (fromIntegral (length falls))
    return $ zip (zip (map getIndex falls) (repeat doubles'')) (repeat prob'') where
        tryGoJail (_, 3) = ("JAIL", 0)
        tryGoJail x = x

go :: StateArray -> StateArray
go probs = runSTArray $ do
    ret <- newArray ((0, 0), (nSquare - 1, 2)) 0 :: ST s (STArray s State Double)
    forM_ [0 .. nSquare - 1] $ \sqr -> do
        forM_ [0 .. 2] $ \doubles -> do
            let expands = go' (sqr, doubles) (probs!(sqr, doubles))
            forM_ expands $ \((sqr', doubles'), prob') -> do
                old <- readArray ret (sqr', doubles') 
                writeArray ret (sqr', doubles') (old + prob')
    return ret

goIter :: StateArray -> Int -> StateArray
goIter probs 0 = probs
goIter probs count = goIter (go probs) (count - 1)

solve :: Array Int Double
solve = runSTArray $ do
    ret <- newArray (0, nSquare - 1) 0 :: ST s (STArray s Int Double)
    let probs = goIter init 233
    forM_ [0 .. nSquare - 1] $ \sqr -> do
        writeArray ret sqr ((probs!(sqr, 0)) + (probs!(sqr, 1)) + (probs!(sqr, 2)))
    return ret
    where init = listArray ((0, 0), (nSquare - 1, 2)) (1 : repeat 0)

main = putStrLn $ concat $ map (printNum . fst) $ take 3 $ reverse $ sortBy (compare `on` snd) (assocs solve) where
    printNum :: Int -> String
    printNum = printf "%02d"
