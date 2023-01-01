module PrimeFactors (primeFactors) where

import qualified Data.List as L

primeFactors :: Integer -> [Integer]
primeFactors = go primes
  where
    primes = L.unfoldr nxtPrime 1
    nxtPrime k = Just (prime, prime + 1)
      where
        prime = head $ dropWhile (not . isPrime) [k ..]
    isPrime k = (k > 1) && null [x | x <- [2 .. isqrt k], k `mod` x == 0]
    isqrt x = floor . sqrt $ (fromIntegral x :: Float)
    go [] _ = error "no primes"
    go xs@(k : ys) x
      | x > 1 && x `mod` k == 0 = k : go xs (x `div` k)
      | x <= 1 = []
      | otherwise = go ys x
