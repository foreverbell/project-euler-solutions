{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies #-}

module Common.NumMod.NumMod (
  IntMod
, fromInteger_
, fromInt
, toInt
) where

import           Prelude
import qualified Prelude as P
import           Data.Vector.Unboxed.Deriving

data IntMod = IntMod {-# UNPACK #-} !Int {-# UNPACK #-} !Int

instance Show IntMod where
  show (IntMod n _) = show n

instance Num IntMod where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE fromInteger #-}

  (+) x (IntMod 0 0) = x
  (+) (IntMod 0 0) x = x
  (+) (IntMod a m1) (IntMod b m2) =
    if m1 == m2 && m1 /= 0
       then IntMod ((a + b) `mod` m1) m1
       else undefined

  (-) x (IntMod 0 0) = x
  (-) (IntMod 0 0) (IntMod b m) = if m /= 0 then IntMod ((-b) `mod` m) m else undefined
  (-) (IntMod a m1) (IntMod b m2) =
    if m1 == m2 && m1 /= 0
       then IntMod ((a - b) `mod` m1) m1
       else undefined

  (*) (IntMod 0 0)  (IntMod 0 0)  = IntMod 0 0
  (*) (IntMod a m)  (IntMod b 0)  = IntMod ((a * b) `mod` m) m
  (*) (IntMod a 0)  (IntMod b m)  = IntMod ((a * b) `mod` m) m
  (*) (IntMod a m1) (IntMod b m2) =
    if m1 == m2 && m1 /= 0
       then if m1 >= 2^31
              then IntMod (fromInteger $ (toInteger a * toInteger b) `mod` toInteger m1) m1
              else IntMod ((a * b) `mod` m1) m1
       else undefined

  -- Wild 0/1
  fromInteger 0 = IntMod 0 0
  fromInteger 1 = IntMod 1 0
  fromInteger _ = undefined

derivingUnbox "IntMod"
  [t| IntMod -> (Int, Int) |]
  [| \(IntMod n m) -> (n, m) |]
  [| \(n, m) -> IntMod n m |] -- unbox deriving is transparent to users, n `mod` m is not needed.

{-# INLINE fromInteger_ #-}
{-# INLINE fromInt #-}
{-# INLINE toInt #-}

fromInteger_ :: Int -> Integer -> IntMod
fromInteger_ m a | m > 0 = IntMod (P.fromInteger $ a `mod` m') m
                 | otherwise = error "invalid modulo"
  where
    m' = P.toInteger m

fromInt :: Int -> Int -> IntMod
fromInt m a | m > 0 = IntMod (a `mod` m) m
            | otherwise = error "invalid modulo"

toInt :: IntMod -> Int
toInt (IntMod n _) = n
