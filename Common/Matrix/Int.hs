-- modified from https://github.com/Daniel-Diaz/matrix for my specific use.
    
module Common.Matrix.Int (
    Matrix,
    rows, cols,
    fmap,
    (!), (!.), getElem, safeGet, unsafeGet,
    getRow, getCol,
    fromList, toList,
    create,
    zero, identity, scalar,
    add, subtract, multiply,
    power,
    createOps
) where

import           Prelude hiding (subtract, negate, fmap)
import           Control.DeepSeq
import           Data.Bits (Bits, shiftR, (.&.))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as RV

data Matrix = Matrix {
    rows :: Int,
    cols :: Int,
    vect :: V.Vector Int
} deriving (Eq, Show)

instance NFData (Matrix) where
    rnf = rnf . vect

encode :: Int -> (Int, Int) -> Int
{-# INLINE encode #-}
encode m (i, j) = (i - 1) * m + j - 1

fmap :: (Int -> Int) -> Matrix -> Matrix
{-# INLINE fmap #-}
fmap f m@(Matrix r c v) = Matrix r c $ V.map f v

getElem :: Int -> Int -> Matrix -> Int
{-# INLINE getElem #-}
getElem i j m = case safeGet i j m of
    Just x -> x
    Nothing -> error "getElem: out of bound."

(!) :: Matrix -> (Int, Int) -> Int
{-# INLINE (!) #-}
m ! (i, j) = getElem i j m

(!.) :: Matrix -> (Int, Int) -> Int
{-# INLINE (!.) #-}
m !. (i, j) = unsafeGet i j m

safeGet :: Int -> Int -> Matrix -> Maybe Int
{-# INLINE safeGet #-}
safeGet i j m@(Matrix r c _) 
    | i < 1 || j < 1 || i > r || j > c = Nothing
    | otherwise = Just $ unsafeGet i j m

unsafeGet :: Int -> Int -> Matrix -> Int
{-# INLINE unsafeGet #-}
unsafeGet i j m@(Matrix r c v) = V.unsafeIndex v $ encode c (i, j)

getRow :: Int -> Matrix -> V.Vector Int
{-# INLINE getRow #-}
getRow i (Matrix _ m v) = V.slice (m * (i - 1)) m v

getCol :: Int -> Matrix -> V.Vector Int
{-# INLINE getCol #-}
getCol j (Matrix n m v) = V.generate n $ \i -> v V.! encode m (i + 1, j)

create :: Int -> Int -> ((Int, Int) -> Int) -> Matrix
{-# INLINE create #-}
create n m f = Matrix n m $ V.fromList [ f (i, j) | i <- [1 .. n], j <- [1 .. m] ]

fromList :: Int -> Int -> [Int] -> Matrix
{-# INLINE fromList #-}
fromList n m = Matrix n m . V.fromListN (n * m)

fromLists :: [[Int]] -> Matrix
{-# INLINE fromLists #-}
fromLists [] = error "fromLists: empty list."
fromLists (xs:xss) = fromList n m $ concat $ xs : map (take m) xss where
    n = 1 + length xss
    m = length xs

toList :: Matrix -> [Int]
{-# INLINE toList #-}
toList m@(Matrix r c _) = [ unsafeGet i j m | i <- [1 .. r] , j <- [1 .. c] ]

toLists :: Matrix -> [[Int]]
{-# INLINE toLists #-}
toLists m@(Matrix r c _) = [ [ unsafeGet i j m | j <- [1 .. c] ] | i <- [1 .. r] ]

zero :: Int -> Int -> Matrix
{-# INLINE zero #-}
zero n m = Matrix n m $ V.replicate (n * m) 0

scalar :: Int -> Int -> Matrix
{-# INLINE scalar #-}
scalar n x = create n n $ \(i, j) -> if i == j then x else 0

identity :: Int -> Matrix
{-# INLINE identity #-}
identity n = scalar n 1

add :: Int -> Matrix -> Matrix -> Matrix
{-# INLINE add #-}
add modulo m1@(Matrix r1 c1 v1) m2@(Matrix r2 c2 v2)
    | r1 == r2 && c1 == c2 = Matrix r1 c1 $ V.zipWith h v1 v2
    | otherwise = error $ "add: matrix size not match."
    where h a b = (a + b) `rem` modulo

subtract :: Int -> Matrix -> Matrix -> Matrix
{-# INLINE subtract #-}
subtract modulo m1@(Matrix r1 c1 v1) m2@(Matrix r2 c2 v2)
    | r1 == r2 && c1 == c2 = Matrix r1 c1 $ V.zipWith h v1 v2
    | otherwise = error $ "subtract: matrix size not match."
    where h a b = (a - b) `rem` modulo

multiply :: Int -> Matrix -> Matrix -> Matrix
{-# INLINE multiply #-}
multiply modulo m1@(Matrix _ c _) m2@(Matrix r _ _)
    | c == r = multiply' modulo m1 m2
    | otherwise = error $ "multiply: matrix size not match."

multiply' :: Int -> Matrix -> Matrix -> Matrix
{-# INLINE multiply' #-}
multiply' modulo m1@(Matrix r _ _) m2@(Matrix _ c _) = create r c $ \(i, j) -> dotProduct (RV.unsafeIndex avs $ i - 1) (RV.unsafeIndex bvs $ j - 1) where
    avs = RV.generate r $ \i -> getRow (i + 1) m1
    bvs = RV.generate c $ \i -> getCol (i + 1) m2
    dotProduct v1 v2 = V.foldl' (\a b -> (a + b) `rem` modulo) 0 $ V.zipWith (\a b -> (a * b) `rem` modulo) v1 v2

power :: (Integral a, Bits a) => Int -> a -> Matrix -> Matrix
{-# INLINE power #-}
power modulo p m@(Matrix r c _) 
    | r == c = helper m p $ identity r 
    | otherwise = error $ "power: matrix not squared."
    where
        helper _ 0 ret = ret
        helper m p ret = if ((p .&. 1) == 1)
            then helper m' p' (multiply' modulo ret m)
            else helper m' p' ret where
                m' = multiply' modulo m m
                p' = p `shiftR` 1

{-# INLINE createOps #-}
createOps modulo = (add modulo, subtract modulo, multiply modulo, power modulo)
