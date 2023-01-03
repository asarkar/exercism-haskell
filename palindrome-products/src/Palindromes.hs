module Palindromes (largestPalindrome, smallestPalindrome) where

import Control.Arrow ((&&&))
import qualified Control.Monad as M

{-
Let palindrome = first * second
 = (10^n - x) * (10^n - y), where x and y are positive integers
 = 10^(2n) - 10^n * (x + y) + xy
 = 10^n * (10^n — (x + y)) + xy
 = 10^n * left + right, where left = 10^n — (x + y) and right = xy ---(i)

Define z = x + y ---(ii)
left = 10^n — z
right = x(z-x) using (i) and (ii)
 = -x^2 + zx
Therefore, x^2 - zx + right = 0 ---(iii)

Now, two n digits numbers could generate a 2n - 1 or 2n digits palindrome.
For example, palindrome number 'abcddcba' for n=4 can be written as:
 10^4*abcd + dcba
 = 10^4*left + right.

Thus, we can substitute for right the reverse of left in the equation (iii).
From (i) and (ii), left = 10^n — z > 0. Since we are given min and max,
min <= 10^n — z <= max
 => 10^n - max <= z <= 10^n - min.
-}
type Palindrome = (Integer, [(Integer, Integer)])

largestPalindrome :: Integer -> Integer -> Maybe Palindrome
largestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | n == 1 = zm $ Just 9
  | otherwise = zm $ M.msum $ map (lp hi) rng
  where
    n = (length . show) maxFactor
    hi = 10 ^ n
    rng = [(hi - maxFactor) .. (hi - minFactor)]
    zm = fmap (id &&& fact minFactor maxFactor)

lp :: Integer -> Integer -> Maybe Integer
lp hi x
  | d >= 0 && isInt root1 && isInt root2 = Just $ hi * left + right
  | otherwise = Nothing
  where
    isInt i = i == fromInteger (round i)
    toF i = fromIntegral i :: Float
    left = hi - x
    right = torevI left
    x' = toF x
    d = (x' * x') - toF (4 * right)
    y = sqrt d
    root1 = (x' + y) / 2.0
    root2 = (x' - y) / 2.0

torevI :: Integer -> Integer
torevI x = read ((reverse . show) x) :: Integer

fact :: Integer -> Integer -> Integer -> [(Integer, Integer)]
fact minFactor maxFactor n = factors
  where
    rng = [minFactor .. maxFactor]
    inRng x = x >= minFactor && x <= maxFactor
    factors = [(x, i) | x <- rng, let (i, j) = divMod n x, j == 0 && inRng i]

smallestPalindrome :: Integer -> Integer -> Maybe Palindrome
smallestPalindrome minFactor maxFactor
  | minFactor > maxFactor = Nothing
  | n == 1 = zm $ Just 1
  | otherwise = zm $ M.msum $ map (sp hi n minFactor maxFactor) rng
  where
    n = (length . show) maxFactor
    hi = 10 ^ n
    rng = [hi - minFactor, hi - minFactor - 1 .. hi - maxFactor]
    zm = fmap (id &&& fact minFactor maxFactor)

{-
Recall that for a quadratic equation ax^2 + bx + c = 0,
the two solutions are given by (-b ± √(b^2 - 4ac)) / (2a),
which has at least one real solution if the discriminant (b^2 - 4ac) >= 0.

Solve x^2 - zx + right = 0.
a = 1, b = -z, c = right.
Discriminant d = (b^2 - 4ac) = z^2 - 4 * right.
Roots: (-b ± √d) / 2a = (z ± √d) / 2
-}
sp :: Integer -> Int -> Integer -> Integer -> Integer -> Maybe Integer
sp hi n minFactor maxFactor x
  | inRng left && inRng right = Just ((10 ^ (n - 1)) * left + right)
  | otherwise = Nothing
  where
    left = hi - x
    right = torevI left
    inRng i = i >= minFactor && i <= maxFactor
