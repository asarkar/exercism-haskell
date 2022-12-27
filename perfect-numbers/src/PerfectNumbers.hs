{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

{-
 It can be shown that the sum of the divisors of a number
 can be expressed as the product of the sum of the powers
 of the prime factors of the number.

 Example:
 n = 18, sum of divisors = 1 + 2 + 3 + 6 + 9 + 18
 = 2^0 x 3^0 + 2^1 x 3^0 + 2^0 x 3^1 +
   2^1 x 361 + 2^0 x 3^2 + 2^1 x 3^2
 = (2^0 + 2^1) x (3^0 + 3^1 + 3^2)
 = (1 + p1) x (1 + p2 + p2^2), where p1=2, and p2=3

 So, the task reduces to finding all the prime factors
 and the product of the sum of their powers.

 Furthermore, the highest power of a factor is the
 number of times it divides the "remaining" number.
 "Remaining" means the result after progressively
 dividing the original number with the smaller prime
 factors.

 In the example above, the highest power of 2 is 1.
 The highest power of 3 is 2.
 -}
classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | n <= 3 = Just Deficient
  | otherwise = Just $ case compare pdt3 n of
      LT -> Deficient
      GT -> Abundant
      _ -> Perfect
  where
    -- There is no prime factor greater than sqrt n
    rt = iSqrt n
    (_ : xs) = scanl sumOfPowers (1, n) [2 .. rt]
    -- Remaining n
    y = snd $ last xs
    pdt1 = product $ map fst xs
    {-
    This condition is to handle the case when remaining
    n is a prime number greater than 2.

    For example, when original n = 6, sqrt = 2, and
    remaining n = 3.
    -}
    pdt2 = pdt1 * (if y > 2 then 1 + y else 1)
    -- Exclude the number from the sum
    pdt3 = pdt2 - n

iSqrt :: Int -> Int
iSqrt n = floor $ sqrt x :: Int
  where
    x = fromIntegral n :: Float

{-
First element of the returned tuple is the
sum of powers.
Second element is the remaining n.
-}
sumOfPowers :: (Int, Int) -> Int -> (Int, Int)
sumOfPowers (k, n) i
  | remainder == 0 = (i ^ k + x, y)
  | otherwise = (1, n)
  where
    (quotient, remainder) = divMod n i
    (x, y) = sumOfPowers (k + 1, quotient) i
