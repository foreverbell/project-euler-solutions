import Common.Numbers.EulerPhi (phiTo)
import Common.Numbers.Primes (testPrime)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (forM_)

phiChainLength :: Int -> [Int]
phiChainLength n = V.toList $ V.create $ do
  r <- MV.new (n + 1)
  MV.write r 1 1
  forM_ [2 .. n] $ \i -> do
    let j = phis V.! i
    v <- MV.read r j
    MV.write r i (v + 1)
  return r
  where
    phis = V.fromList (0 : phiTo n)

solve n = sum $ map fst ret
  where
    ret = filter (\p -> snd p == 25 && testPrime (fst p)) $ zip [0 ..] (phiChainLength n)

main = print $ solve 40000000
