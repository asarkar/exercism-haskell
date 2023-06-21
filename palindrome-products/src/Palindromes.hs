module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import qualified Control.Monad as M
import qualified Data.Maybe as Mb

type Palindrome = (Integer, [(Integer, Integer)])

largestPalindrome :: Integer -> Integer -> Maybe Palindrome
largestPalindrome minFactor maxFactor =
  palindrome maxFactor minFactor (-1) maxFactor maxFactor False Nothing

smallestPalindrome :: Integer -> Integer -> Maybe Palindrome
smallestPalindrome minFactor maxFactor =
  palindrome minFactor maxFactor 1 minFactor minFactor False Nothing

{-
Searches for a palindrome with factors in the given range.
Whether the palindrome is the maximum or minimum depends
on the given step.
-}
palindrome ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  {-
  This variable determines whether the last iterations of the inner
  loop (right) produced a product that satisfied the given condition
  when compared with the palindrome found so far. Since the ranges are
  monotonically increasing/decreasing, if no such product was found
  in the last iteration, it won't be found in any future iterations
  either, so, we can stop.
  -}
  Bool ->
  Maybe Palindrome ->
  Maybe Palindrome
palindrome start end step left right continue pal
  -- Done with the outer loop.
  | signum (end - left) == -step = pal
  -- One factor is smaller or equal to the other.
  | signum (left - right) == -step =
      if continue
        then go (left + step) start False pal
        else pal
  -- Below guards continue the inner loop.
  | shouldTake && isPal = go left (right + step) shouldContinue justPal
  | otherwise = go left (right + step) shouldContinue pal
  where
    op = if step == 1 then (<=) else (>=)
    pdt = left * right
    shouldTake = Mb.isNothing $ M.mfilter (not . op pdt . fst) pal
    shouldContinue = continue || shouldTake
    x = show pdt
    isPal = x == reverse x
    {-
    If the newly found palindrome is not the same as the one found
    before, we need to reset the factors. One palindrome may have
    multiple factors, like 9 has [1, 9] and [3, 3].
    -}
    newFact (p, fact) = if p == pdt then fact else []
    newPal = fmap (\p -> (pdt, (left, right) : newFact p)) pal
    justPal = M.msum [newPal, Just (pdt, [(left, right)])]
    go = palindrome start end step
