-- modified from https://github.com/Daniel-Diaz/matrix for my specific use.
    
module Common.Matrix.Int (
    Matrix,
    rows, cols,
    fmap,
    (!), (!.), getElem, safeGet, unsafeGet,
    getRow, getCol,
    fromList, toList, fromLists, toLists,
    create,
    zero, identity, scalar,
    add, subtract, multiply,
    power,
    bind, bind'
) where

import           Prelude hiding (subtract, fmap)
import           Control.DeepSeq
import           Data.Bits (Bits, shiftR, (.&.))
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as RV

data Matrix = Matrix {
    rows :: {-# UNPACK #-} !Int,
    cols :: {-# UNPACK #-} !Int,
    vect :: V.Vector Int
} deriving (Eq, Show)

instance NFData (Matrix) where
    rnf = rnf . vect

encode :: Int -> (Int, Int) -> Int
{-# INLINE encode #-}
encode m (i, j) = (i - 1) * m + j - 1

fmap :: (Int -> Int) -> Matrix -> Matrix
{-# INLINE fmap #-}
fmap f (Matrix r c v) = Matrix r c $ V.map f v

getElem :: Int -> Int -> Matrix -> Int
{-# INLINE getElem #-}
getElem i j m = fromMaybe (error "getElem: out of bound.") (safeGet i j m)

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
unsafeGet i j (Matrix _ c v) = V.unsafeIndex v $ encode c (i, j)

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

add :: (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
{-# INLINE add #-}
add af (Matrix r1 c1 v1) (Matrix r2 c2 v2)
    | r1 == r2 && c1 == c2 = Matrix r1 c1 $ V.zipWith af v1 v2
    | otherwise = error "add: matrix size not match."

subtract :: (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
{-# INLINE subtract #-}
subtract af m1 m2 = add (\a b -> af a (-b)) m1 m2

multiply :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
{-# INLINE multiply #-}
multiply af mf m1@(Matrix _ c _) m2@(Matrix r _ _)
    | c == r = multiply' af mf m1 m2
    | otherwise = error "multiply: matrix size not match."

multiply' :: (Int -> Int -> Int) -> (Int -> Int -> Int) -> Matrix -> Matrix -> Matrix
{-# INLINE multiply' #-}
multiply' af mf m1@(Matrix r _ _) m2@(Matrix _ c _) = create r c $ \(i, j) -> dotProduct (RV.unsafeIndex avs $ i - 1) (RV.unsafeIndex bvs $ j - 1) where
    avs = RV.generate r $ \i -> getRow (i + 1) m1
    bvs = RV.generate c $ \i -> getCol (i + 1) m2
    dotProduct v1 v2 = V.foldl' af 0 $ V.zipWith mf v1 v2

power :: (Integral a, Bits a) => (Int -> Int -> Int) -> (Int -> Int -> Int) -> Matrix -> a -> Matrix
{-# INLINE power #-}
power af mf m@(Matrix r c _) p
    | r == c = helper m p $ identity r 
    | otherwise = error "power: matrix not squared."
    where
        mult = multiply' af mf
        helper _ 0 ret = ret
        helper a x ret = if (x .&. 1) == 1
            then helper a' x' (mult ret a)
            else helper a' x' ret where
                a' = mult a a
                x' = x `shiftR` 1

madd :: Int -> Int -> Int -> Int
{-# INLINE madd #-}
madd m a b = (a + b) `rem` m

mmul :: Int -> Int -> Int -> Int 
{-# INLINE mmul #-}
mmul m a b = (a * b) `rem` m

bind :: (Integral a, Bits a) => Int -> (Matrix -> Matrix -> Matrix, Matrix -> Matrix -> Matrix, Matrix -> Matrix -> Matrix, Matrix -> a -> Matrix)
bind m = (add (madd m), subtract (madd m), multiply (madd m) (mmul m), power (madd m) (mmul m))

bind' :: (Integral a, Bits a) => (Matrix -> Matrix -> Matrix, Matrix -> Matrix -> Matrix, Matrix -> Matrix -> Matrix, Matrix -> a -> Matrix)
bind'  = (add (+), subtract (+), multiply (+) (*), power (+) (*))
