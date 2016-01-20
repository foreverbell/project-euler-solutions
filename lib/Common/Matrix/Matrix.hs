-- modified from https://github.com/Daniel-Diaz/matrix for my specific use.
    
module Common.Matrix.Matrix (
    Matrix,
    rows, cols,
    fmap,
    (!), (!.), getElem, safeGet, unsafeGet,
    getRow, getCol,
    fromList, toList, fromLists, toLists,
    create,
    zero, identity, scalar,
    add, subtract, multiply,
    power
) where

import           Prelude hiding (subtract, fmap)
import           Data.Bits (Bits, shiftR, (.&.))
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as RV

data V.Unbox a => Matrix a = Matrix {
    rows :: {-# UNPACK #-} !Int,
    cols :: {-# UNPACK #-} !Int,
    vect :: V.Vector a
} deriving (Eq, Show)

encode :: Int -> (Int, Int) -> Int
{-# INLINE encode #-}
encode m (i, j) = (i - 1) * m + j - 1

fmap :: V.Unbox a => (a -> a) -> Matrix a -> Matrix a
{-# INLINE fmap #-}
fmap f (Matrix r c v) = Matrix r c $ V.map f v

getElem :: V.Unbox a => Int -> Int -> Matrix a -> a
{-# INLINE getElem #-}
getElem i j m = fromMaybe (error "getElem: out of bound.") (safeGet i j m)

(!) :: V.Unbox a => Matrix a -> (Int, Int) -> a
{-# INLINE (!) #-}
m ! (i, j) = getElem i j m

(!.) :: V.Unbox a => Matrix a -> (Int, Int) -> a
{-# INLINE (!.) #-}
m !. (i, j) = unsafeGet i j m

safeGet :: V.Unbox a => Int -> Int -> Matrix a -> Maybe a
{-# INLINE safeGet #-}
safeGet i j m@(Matrix r c _) 
    | i < 1 || j < 1 || i > r || j > c = Nothing
    | otherwise = Just $ unsafeGet i j m

unsafeGet :: V.Unbox a => Int -> Int -> Matrix a -> a
{-# INLINE unsafeGet #-}
unsafeGet i j (Matrix _ c v) = V.unsafeIndex v $ encode c (i, j)

getRow :: V.Unbox a => Int -> Matrix a -> V.Vector a
{-# INLINE getRow #-}
getRow i (Matrix _ m v) = V.slice (m * (i - 1)) m v

getCol :: V.Unbox a => Int -> Matrix a -> V.Vector a
{-# INLINE getCol #-}
getCol j (Matrix n m v) = V.generate n $ \i -> v V.! encode m (i + 1, j)

create :: V.Unbox a => Int -> Int -> ((Int, Int) -> a) -> Matrix a
{-# INLINE create #-}
create n m f = Matrix n m $ V.fromList [ f (i, j) | i <- [1 .. n], j <- [1 .. m] ]

fromList :: V.Unbox a => Int -> Int -> [a] -> Matrix a
{-# INLINE fromList #-}
fromList n m = Matrix n m . V.fromListN (n * m)

fromLists :: V.Unbox a => [[a]] -> Matrix a
{-# INLINE fromLists #-}
fromLists [] = error "fromLists: empty list."
fromLists (xs:xss) = fromList n m $ concat $ xs : map (take m) xss where
    n = 1 + length xss
    m = length xs

toList :: V.Unbox a => Matrix a -> [a]
{-# INLINE toList #-}
toList m@(Matrix r c _) = [ unsafeGet i j m | i <- [1 .. r] , j <- [1 .. c] ]

toLists :: V.Unbox a => Matrix a -> [[a]]
{-# INLINE toLists #-}
toLists m@(Matrix r c _) = [ [ unsafeGet i j m | j <- [1 .. c] ] | i <- [1 .. r] ]

zero :: (V.Unbox a, Num a) => Int -> Int -> Matrix a
{-# INLINE zero #-}
zero n m = Matrix n m $ V.replicate (n * m) 0

scalar :: (V.Unbox a, Num a) => Int -> a -> Matrix a
{-# INLINE scalar #-}
scalar n x = create n n $ \(i, j) -> if i == j then x else 0

identity :: (V.Unbox a, Num a) => Int -> Matrix a
{-# INLINE identity #-}
identity n = scalar n 1

add :: (V.Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE add #-}
add (Matrix r1 c1 v1) (Matrix r2 c2 v2)
    | r1 == r2 && c1 == c2 = Matrix r1 c1 $ V.zipWith (+) v1 v2
    | otherwise = error "add: matrix size not match."

subtract :: (V.Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE subtract #-}
subtract (Matrix r1 c1 v1) (Matrix r2 c2 v2)
    | r1 == r2 && c1 == c2 = Matrix r1 c1 $ V.zipWith (-) v1 v2
    | otherwise = error "subtract: matrix size not match."

multiply :: (V.Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multiply #-}
multiply m1@(Matrix _ c _) m2@(Matrix r _ _)
    | c == r = multiply' m1 m2
    | otherwise = error "multiply: matrix size not match."

multiply' :: (V.Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
{-# INLINE multiply' #-}
multiply' m1@(Matrix r _ _) m2@(Matrix _ c _) = create r c $ \(i, j) -> dotProduct (RV.unsafeIndex avs $ i - 1) (RV.unsafeIndex bvs $ j - 1) where
    avs = RV.generate r $ \i -> getRow (i + 1) m1
    bvs = RV.generate c $ \i -> getCol (i + 1) m2
    dotProduct v1 v2 = V.foldl' (+) 0 $ V.zipWith (*) v1 v2

power :: (Integral a, Bits a, V.Unbox b, Num b) => Matrix b -> a -> Matrix b
{-# INLINE power #-}
power m@(Matrix r c _) p
    | r == c = helper m p $ identity r 
    | otherwise = error "power: matrix not squared."
    where
        helper _ 0 ret = ret
        helper a x ret = if (x .&. 1) == 1
            then helper a' x' (multiply' ret a)
            else helper a' x' ret where
                a' = multiply' a a
                x' = x `shiftR` 1
