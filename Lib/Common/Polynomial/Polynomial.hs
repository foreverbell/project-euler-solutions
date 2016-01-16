module Common.Polynomial.Polynomial (
  Polynomial
, toList
, fromList
, (!)
, naiveMultiply
, karatsubaMultiply
) where

import           Data.Bits (shiftL, shiftR)
import qualified Data.Vector.Unboxed as V

newtype V.Unbox a => Polynomial a = P (V.Vector a)
  deriving (Show)

instance (V.Unbox a, Num a) => Num (Polynomial a) where
  (+) (P p1) (P p2) = P $ zipWith_ (+) p1 p2
  (-) (P p1) (P p2) = P $ zipWith_ (-) p1 p2
  (*) p1 p2 = karatsubaMultiply p1 p2
  fromInteger n = fromList [fromInteger n]
  abs _ = undefined
  signum _ = undefined

toList :: V.Unbox a => Polynomial a -> [a]
toList (P p) = V.toList p

fromList :: V.Unbox a => [a] -> Polynomial a
fromList = P . V.fromList

(!) :: (V.Unbox a, Num a) => Polynomial a -> Int -> a
P p ! i = p V.! i

(?!) :: (V.Unbox a, Num a) => V.Vector a -> Int -> a
p ?! i | i < 0 || i >= n = 0
       | otherwise = V.unsafeIndex p i
  where n = V.length p

shift :: (V.Unbox a, Num a) => Int -> V.Vector a -> V.Vector a
shift n = (V.++) (V.replicate n 0)

zipWith_ :: (V.Unbox a, Num a) => (a -> a -> a) -> V.Vector a -> V.Vector a -> V.Vector a 
zipWith_ f p1 p2 = V.generate (max (V.length p1) (V.length p2)) $ \i -> f (p1 ?! i) (p2 ?! i)

zipWith3_ :: (V.Unbox a, Num a) => (a -> a -> a -> a) -> V.Vector a -> V.Vector a -> V.Vector a -> V.Vector a
zipWith3_ f p1 p2 p3 = V.generate (maximum (map V.length [p1, p2, p3])) $ \i -> f (p1 ?! i) (p2 ?! i) (p3 ?! i)

naiveMultiply :: (V.Unbox a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
naiveMultiply (P p1) (P p2) | n == 0 || m == 0 = P V.empty
                            | otherwise = P $ V.fromList $
  flip map [0 .. n + m - 2] $ \k -> do
    let offset = max 0 (k + 1 - m)
    V.sum $ V.imap (get offset k) (V.drop offset $ V.take (k + 1) p1)
  where
    n = V.length p1
    m = V.length p2
    get offset k i v = (V.unsafeIndex p2 j) * v
      where j = k - i - offset

karatsubaMultiply :: (V.Unbox a, Num a) => Polynomial a -> Polynomial a -> Polynomial a
karatsubaMultiply (P p1) (P p2) | m == 0 = P V.empty
                                | n <= 250 = naiveMultiply (P p1) (P p2)
                                | otherwise = P $ V.take deg $ zipWith3_ (\x y z -> x + y + z) sub1 part1 part2
  where
    n = max (V.length p1) (V.length p2)
    m = min (V.length p1) (V.length p2)
    deg = V.length p1 + V.length p2 - 1
    half = n `shiftR` 1 + 1
    a = V.take half p1
    b = V.drop half p1
    c = V.take half p2
    d = V.drop half p2
    P sub1 = karatsubaMultiply (P a) (P c)
    P sub2 = karatsubaMultiply (P b) (P d)
    P sub3 = karatsubaMultiply (P $ zipWith_ (+) a b) (P $ zipWith_ (+) c d)
    part1 = shift half $ zipWith3_ (\x y z -> x - y - z) sub3 sub2 sub1
    part2 = shift (half `shiftL` 1) sub2
