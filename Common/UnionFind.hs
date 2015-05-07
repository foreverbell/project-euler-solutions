{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Common.UnionFind (
    UFSet(..),
    display,
    make,
    find,
    union
) where

import qualified Common.Ref as R
import qualified Data.Array.MArray as MA
import Control.Monad (liftM2)

data UFSet a m = UFSet {
    ufsSize :: R.Ref m Int,
    ufsRange :: (Int, Int),
    ufsSet :: a Int Int
}

display :: (MA.MArray a Int m) => UFSet a m -> m String
display ufs = do
    size <- R.read $ ufsSize ufs
    let range@(l, r) = ufsRange ufs
    let set = ufsSet ufs
    elems <- sequence $ map (\i -> MA.readArray set i) [l .. r]
    return $ show (size, range, zip [l .. r] elems)

make :: (MA.MArray a Int m, R.R m) => (Int, Int) -> m (UFSet a m) 
make (l, r) = liftM2 (\s a -> UFSet s (l, r) a) (R.new (r - l + 1)) (MA.newListArray (l, r) [l .. r])

find :: (MA.MArray a Int m) => UFSet a m -> Int -> m Int
find ufs u = do
    father <- MA.readArray (ufsSet ufs) u
    if u == father
        then return u
        else do
            father' <- find ufs father
            MA.writeArray (ufsSet ufs) u father'
            return father'

union :: (MA.MArray a Int m, R.R m) => UFSet a m -> Int -> Int -> m Bool
union ufs u v = do
    u' <- find ufs u
    v' <- find ufs v
    if (u' /= v')
        then do
            MA.writeArray (ufsSet ufs) u' v' 
            R.modify (ufsSize ufs) pred
            return True
        else return False
