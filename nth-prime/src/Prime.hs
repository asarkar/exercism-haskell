{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Prime (nth) where

import Data.Map (Map)
import qualified Data.Map as M

nth :: Int -> Maybe Integer
nth n
  | n == 0 = Nothing
  | otherwise = Just $ nth' (n - 1) 2 M.empty

{-
We use a modified Sieve of Eratosthenes algorithm.

We inspect one natural number at a time, and do one of the following:
1. If we find a prime, we insert its square in a map, along with the
   prime as the value, so that we can generate the multiples if we need to.

2. If we find a composite, we remove all the primes that are its factors,
   and insert the next multiples in the map.
-}
nth' :: Int -> Integer -> Map Integer [Integer] -> Integer
nth' n x composites
  -- Prime
  | M.notMember x composites =
      -- Insert square of the prime
      let c = M.insertWith (++) (x * x) [x] composites
          y = nth' (n - 1) (x + 1) c
       in (if n == 0 then x else y)
  -- Composite
  | otherwise =
      -- Delete the composite and get its prime factors
      let (Just primes, c) = M.updateLookupWithKey (\_ _ -> Nothing) x composites
          -- Insert multiples of the prime factors
          ins y = M.insertWith (++) (x + y) [y]
          c' = foldl (flip ins) c primes
       in nth' n (x + 1) c'
