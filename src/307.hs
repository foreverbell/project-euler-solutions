import Text.Printf (printf)
import qualified Data.Vector.Unboxed as V

logs :: V.Vector Double
logs = V.fromList ps
  where 
    xs = [ log (fromIntegral k) | k <- [1 .. 1000000] ]
    ps = zipWith (+) xs (0:ps)

logSum :: Int -> Int -> Double
logSum a b | a > b = 0
           | otherwise = ask b - ask (a-1)
  where ask 0 = 0
        ask k = logs V.! (k-1)

binomial_log :: Int -> Int -> Double
binomial_log n k = logSum (n - k + 1) n - logSum 1 k

permutation_log :: Int -> Int -> Double
permutation_log n k = logSum (n - k + 1) n

solve :: Int -> Int -> Double
solve n k = 1 - below2
  where
    total = (fromIntegral k) * (log $ fromIntegral n)
    below2 = sum $ map f [0 .. k `div` 2]
    f x = exp $ binomial_log n (k-x) + permutation_log (k-x) x + permutation_log k (k-x) - (fromIntegral x) * log 2 - total

main = printf "%.10f\n" $ solve 1000000 20000

{- Since Haskell doesn't provide a `long double` primitive, 
   now this code is suffering from precision issue.
   The following C++ code provides the correct answer.

#include <bits/stdc++.h>

using namespace std;

const int maxN = 1000000 + 10;

typedef long double ld;

ld logs[maxN];

ld binomial_log(int n, int k) {
  return logs[n] - logs[n - k] - logs[k];
}

ld permutation_log(int n, int k) {
  return logs[n] - logs[n - k];
}

int main() {
  for (int i = 1; i < maxN; ++i) {
    logs[i] = logs[i - 1] + log(ld(i));
  }
  int n = 1000000, k = 20000;
  double ret = 1, total = k * log(ld(n));
  for (int i = 0; i <= k / 2; ++i) {
    ret -= exp(binomial_log(n, k - i) + permutation_log(k - i, i) + permutation_log(k, k - i) - i * log(2.0) - total);
  }
  printf("%.10f\n", double(ret));
  return 0;
}

-}
