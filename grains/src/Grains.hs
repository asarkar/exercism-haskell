{-# LANGUAGE NumericUnderscores #-}

module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | (0 < n) && (n <= 64) = Just $ 2 ^ (n - 1)
  | otherwise = Nothing

total :: Integer
total = 18_446_744_073_709_551_615