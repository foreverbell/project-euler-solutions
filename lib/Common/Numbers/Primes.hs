module Common.Numbers.Primes (
  primes
, primes'
, primesTo
, primesTo'
, testPrime
, factorize
, toDivisors
, countPrimeApprox
, countPrime
, countPrime'
) where

import           Control.Arrow (first)
import           Control.Monad (forM_, foldM, when)
import           Control.Monad.ST (runST)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Loop (iterateLoopT, exit) 
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Bits (shiftR, (.&.))
import           Data.List (sort, group)
import           Data.Maybe (isJust)
import           Data.Either (isRight)
import           System.Random (split, randomR, RandomGen)
import           Common.Numbers.Numbers (powMod)
import qualified Common.MonadRef as R
import           Common.Utils (isqrt, if')

primes :: Integral a => [a]
{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}
primes = 2 : eratos [3, 5 .. ] 
  where
    eratos [] = []
    eratos (p:xs) = p : eratos (xs `minus` [p^2, p^2+p .. ])
    minus xs'@(x:xs) ys'@(y:ys) = case compare x y of 
      LT -> x : minus xs ys'
      EQ -> minus xs ys 
      GT -> minus xs' ys
    minus xs _ = xs

primes' :: [Int]
primes' = 2 : filter testPrime [3, 5 .. ]

primesTo :: Int -> [Int]
primesTo = V.toList . snd . primesTo'

primesTo' :: Int -> (V.Vector Bool, V.Vector Int)
primesTo' n = runST $ do
  pt <- R.new (0 :: Int)
  sieve <- MV.replicate (n + 1) True
  primes <- MV.replicate (countPrimeApprox n + 1) (0 :: Int)
  MV.unsafeWrite sieve 0 False
  MV.unsafeWrite sieve 1 False
  forM_ [2 .. n] $ \i -> do
    isPrime <- MV.unsafeRead sieve i
    when isPrime $ do
      pt' <- R.modify' pt (+ 1)
      MV.unsafeWrite primes pt' i
    pt' <- R.read pt
    iterateLoopT 1 $ \j -> 
      if' (j > pt') exit $ do
        p' <- lift $ MV.unsafeRead primes j
        if' (p' * i > n) exit $ do
          lift $ MV.unsafeWrite sieve (p' * i) False
          if' (i `rem` p' == 0) exit (return $ j + 1)
  ptv <- R.read pt
  sieve' <- V.unsafeFreeze sieve
  primes' <- V.unsafeFreeze primes
  return (sieve', V.force $ V.slice 1 ptv primes')

primes10k = primesTo 10000

millerRabinTest :: Int -> Int -> Bool
millerRabinTest n b = ((p == 1) || (p == n - 1) || (n == b)) || ((n `rem` b /= 0) && rec cnt p)
  where
    tail0 x cnt = if (x .&. 1) == 1 
      then (x, cnt)
      else tail0 (x `shiftR` 1) (cnt + 1)
    (m, cnt) = tail0 (n - 1) 0
    p = if n < 2^31
      then powMod b m n
      else fromIntegral $ powMod (toInteger b) (toInteger m) (toInteger n)
    rec 0 _ = False
    rec cnt p = (p2 == n - 1) || rec (cnt - 1) p2 
      where
        p2 = if p < 2^31
            then p^2 `rem` n
            else fromIntegral $ (toInteger p)^2 `rem` (toInteger n)

naiveTest :: Int -> Bool
naiveTest n = all (\d -> n `rem` d /= 0) $ takeWhile (<= root) primes10k
  where
    root = isqrt n

testPrime :: Int -> Bool
testPrime 1 = False
testPrime 2 = True
testPrime 3 = True
testPrime 3215031751 = False
testPrime n 
  | n <= 0 = False
  | n >= 100000000 = all (millerRabinTest n) (takeWhile (< n) b) 
  | otherwise = naiveTest n 
  where 
    b = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37]

pollardRho :: Int -> Int -> Int -> Maybe Int
pollardRho n seed from = case until isRight (\(Left (y, cycle)) -> go cycle cycle y y) (Left (from, 1)) of
                              Right 1 -> Nothing
                              Right x -> Just x
                              _ -> undefined
  where
    g x | n >= 2^31 = fromInteger $ ((toInteger x)^2 + toInteger seed) `rem` toInteger n
        | otherwise = ((x^2 `rem` n) + seed) `rem` n
    go :: Int -> Int -> Int -> Int -> Either (Int, Int) Int
    go cycle 0 _ x = Left (x, cycle * 2)
    go cycle count y x | x' == y = Right 1
                       | d > 1 && d < n = Right d
                       | otherwise = go cycle (count - 1) y x'
      where
        x' = g x
        d = gcd (abs (x' - y)) n

factorize :: RandomGen g => g -> Int -> ([Int], g)
factorize g n = sort `first` helper g n
  where
    helper g 1 = ([], g)
    helper g n | testPrime n = ([n], g)
               | otherwise = (xs ++ ys, g')
      where
        (Just d, g0) = until (isJust . fst) go (Nothing, g)
        (g1, g2) = split g0
        (xs, g') = helper g1 d
        (ys, _)  = helper g2 (n `quot` d)
        go (_, g) = (pollardRho n seed from, g2)
          where
            (seed, g1) = randomR (1, n - 1) g
            (from, g2) = randomR (1, n - 1) g1

toDivisors :: [Int] -> [Int]
toDivisors ps = sort $ foldM (\r xs -> map (*r) xs) 1 (map f (group ps))
  where
    f xs@(x:_) = take (1 + length xs) $ iterate (*x) 1
    f [] = undefined

-- | never underestimated for n <= 10^12
countPrimeApprox :: Int -> Int
countPrimeApprox = truncate . appi . fromIntegral 
  where
    appi x = y - y / 300000 + 7 * l' 
      where
        y = x * l * (1 + l * (1 + l * h))
        w = log x
        l = 1 / w
        l' = log w
        h | x < 10000000  = 2.5625
          | x < 50000000  = 2.5
          | x < 120000000 = 617 / 256
          | x > 10**12    = undefined
          | otherwise     = 2.0625 + l * (3 + l' * l * (13.25 + l' * l * 57.75))

countPrime :: Int -> Int 
countPrime = snd . head . countPrime'

countPrime' :: Int -> [(Int, Int)]
countPrime' n = zip (map snd v) $ V.toList dynamic 
  where
    root = isqrt n
    ps = zip [0 .. ] $ primesTo (root + 1)
    last = n `div` (root + 1)
    v = zip [0 .. ] $ map (\i -> n `div` i) [1 .. root + 1] ++ [last - 1, last - 2 .. 0]
    dynamic = V.create $ do
      dp <- V.thaw $ V.fromList $ map (max 0 . pred . snd) v
      forM_ ps $ \(i, p) -> do
        let p2 = p * p
        forM_ (takeWhile (\(_, k) -> k >= p2) v) $ \(j, k) -> do
          let k' = k `div` p
          let j' = pred $ if' (k' < last) (root + 1 + last - k') (n `div` k')
          v1 <- MV.unsafeRead dp j
          v2 <- MV.unsafeRead dp j'
          MV.unsafeWrite dp j (v1 - v2 + i)
      return dp
