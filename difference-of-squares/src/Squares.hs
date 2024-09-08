module Squares (difference, squareOfSum, sumOfSquares) where

difference :: (Integral a) => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: (Integral a) => a -> a
squareOfSum n = s * s
  where
    s = n * (n + 1) `div` 2

-- // https://helloacm.com/the-difference-between-sum-of-squares-and-square-of-the-sum/
sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = (2 * n + 1) * (n + 1) * n `div` 6
