module Series (Error (..), largestProduct) where

import qualified Data.Char as C
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

type Product = Either Error Integer

largestProduct :: Int -> String -> Product
largestProduct size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | otherwise = lp 0 size 1 (-1) xs
  where
    xs = V.fromList digits

{-
Maintain a sliding window of size n, and a running product.
For every digit that enters the window on the right, one
exits on the left.
new product = (current product) / (exiting digit) * (entering digit)
When exiting digit = 0, calculate the new product by brute force.
A further optimization might be to keep track of any zeros in
the window so as to avoid the brute force product.
-}
lp :: Int -> Int -> Integer -> Integer -> Vector Char -> Product
lp ix n pdt maxPdt xs
  | ix == V.length xs = Right $ max pdt maxPdt
  | otherwise = newPdt >>= (\x -> lp (ix + 1) n x (newMaxPdt x) xs)
  where
    spanIncomplete = ix <= (n - 1)
    newMaxPdt x = if spanIncomplete then -1 else max maxPdt x
    e = (xs !)
    toI x
      | C.isDigit y = Right ((toInteger . C.digitToInt) y)
      | otherwise = Left $ InvalidDigit y
      where
        y = e x
    op f acc x = f acc <$> toI x
    mul = op (*)
    newPdt
      | spanIncomplete = mul pdt ix
      | e j == '0' = lp 0 n 1 (-1) (V.slice (j + 1) n xs)
      | otherwise = op div pdt j >>= (`mul` ix)
      where
        j = ix - n
