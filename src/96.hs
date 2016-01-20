import Data.List (groupBy, sort, sortBy)
import Data.Function (on)
import Data.Bits
import Data.Char (ord)
import Data.Foldable (toList)
import qualified Data.Sequence as S

readInput :: IO [[String]]
readInput = readFile "input/p096_sudoku.txt" >>= (return . process) where
    process dat = map (drop 2) $ groupBy (\x y -> ((head x) == 'G') && ((head y) /= 'G')) $ words dat

getIndex :: Int -> (Int, Int) -> Int
getIndex 0 (x, y) = x * 9 + y
getIndex 1 (x, y) = x
getIndex 2 (x, y) = y
getIndex 3 (x, y) = ((x `div` 3) * 3) + (y `div` 3)

nextCord :: (Int, Int) -> (Int, Int)
nextCord (x, 8) = (x + 1, 0)
nextCord (x, y) = (x, y + 1)

cordList :: [(Int, Int)]
cordList = take 81 $ (0, 0) : map nextCord cordList

dfs :: S.Seq Int -> S.Seq Int -> S.Seq (S.Seq Int) -> [[Int]]
dfs puzzle ret mask
    | null validCandidate = [toList ret]
    | otherwise = concat $ recResults d
    where
        validCandidate :: [([Int], (Int, Int))]
        validCandidate = map (\(x,y,z) -> ((getCandidate mask x y), x)) $ filter (\(x,y,z) -> z == 0) (zip3 cordList (toList puzzle) (toList ret))       
        getCandidate mask cord d = filter helper candidate where
            candidate = [ x | x <- [1 .. 9], (d == 0) || (d == x) ] :: [Int]
            helper d = checkMask mask cord d
        cord@(x,y) = snd $ head $ sortBy (compare `on` (length . fst)) validCandidate
        d = S.index puzzle (getIndex 0 cord)
        rec d
            | checkMask mask cord d = dfs puzzle (updateResult ret d cord) (applyMask mask cord d)
            | otherwise = []
        updateResult ret d cord = S.update (getIndex 0 cord) d ret
        checkMask mask cord d = (not . or) r where
            r = do
                i <- [0 .. 2]
                let m = S.index mask i
                let m = S.index mask i
                let index = getIndex (i + 1) cord
                let bit = testBit (S.index m index) d
                return bit
        applyMask mask cord d = S.fromList r where
            r = do
                i <- [0 .. 2]
                let m = S.index mask i
                let index = getIndex (i + 1) cord
                let set = setBit (S.index m index) d
                return $ S.update index set m
        recResults 0 = [ rec d | d <- [1 .. 9] ]
        recResults d = [ rec d ]

solve :: [String] -> [[Int]]
solve puzzle = dfs sudoku (S.fromList (replicate 81 0)) (S.fromList (replicate 3 setup)) where
    setup = S.fromList (replicate 9 0)
    sudoku = S.fromList $ map (\c -> ord c - ord '0') (concat puzzle)

first3 :: [Int] -> Int
first3 (x:y:z:xs) = x * 100 + y * 10 + z

main = readInput >>= (print . sum . (map (first3 . head . solve)))
