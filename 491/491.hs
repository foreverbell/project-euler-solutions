
import Control.Monad.ST
import Control.Monad (forM_)
import Data.Array
import Data.Array.ST

maskBound :: Int
maskBound = (3^10) - 1

encode :: [Int] -> Int
encode x = foldl helper 0 (zip x [3^i | i <- [0 .. 9]]) where
    helper accum (bit, pow) = accum + bit * pow

decode :: Int -> [Int]
decode x = helper x 10 where
    helper x 0 = []
    helper x n = (x `mod` 3) : (helper (x `div` 3) (n - 1))

increase :: Int -> [Int] -> [Int]
increase digit x = (take digit x) ++ [(x!!digit) + 1] ++ (drop (digit + 1) x)

dynamic :: Int -> Array (Int, Int) Int -> Array (Int, Int) Int
dynamic 0 dp = dp
dynamic p dp = dynamic (p - 1) $ runSTArray $ do
    ret <- newArray ((0, 0), (maskBound, 10)) 0 :: ST s (STArray s (Int, Int) Int)
    forM_ [0 .. maskBound] $ \mask -> do
        let bits = decode mask
        forM_ [0 .. 10] $ \modulo -> do
            let v = dp!(mask, modulo)
            if (v /= 0) 
                then do
                    forM_ [0 .. 9] $ \digit -> do
                        if (((bits!!digit) /= 2) && (p /= 20 || digit /= 0))
                            then do
                                let mask' = encode (increase digit bits)
                                let modulo' = (modulo + ((if (even p) then 10 else 1) * digit)) `mod` 11
                                accumArray ret (mask', modulo') v
                            else return ()
                else return ()
    return ret
    where
        accumArray arr idx delta = do
            x <- readArray arr idx
            writeArray arr idx (x + delta)

solve = (dynamic 20 initArray) ! (maskBound, 0) where
    initArray = runSTArray $ do
        ret <- newArray ((0, 0), (maskBound, 10)) 0
        writeArray ret (0, 0) 1
        return ret

main = print solve
