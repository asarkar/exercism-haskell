module ArmstrongNumbers (armstrong) where

import qualified Data.List as L

armstrong :: Integral a => a -> Bool
armstrong x = sum xs == x
  where
    n = len x
    xs = L.unfoldr (go n) x

go :: Integral a => Integer -> a -> Maybe (a, a)
go _ 0 = Nothing
go n x = Just (d ^ n, x `div` 10)
  where
    d = x `mod` 10

len :: Integral a => a -> Integer
len x
  | x <= 1 = 1
  | otherwise = ceiling (logBase 10 (fromIntegral x :: Float))
