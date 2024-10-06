{-# LANGUAGE TupleSections #-}

module Palindromes
  ( largestPalindrome,
    smallestPalindrome,
  )
where

import qualified Control.Monad as M
import qualified Data.Either as E
import qualified Data.Maybe as Mb

type Palindrome = Maybe (Integer, [(Integer, Integer)])

largestPalindrome :: Integer -> Integer -> Palindrome
largestPalindrome minFactor maxFactor = outer (>=) Nothing rng
  where
    rng = [maxFactor, maxFactor - 1 .. minFactor]

smallestPalindrome :: Integer -> Integer -> Palindrome
smallestPalindrome minFactor maxFactor = outer (<=) Nothing rng
  where
    rng = [minFactor .. maxFactor]

-- outer loop
outer ::
  (Integer -> Integer -> Bool) ->
  Palindrome ->
  [Integer] ->
  Palindrome
outer _ pal [] = pal
outer op pal rng@(l : xs)
  | null inner = pal
  | otherwise = outer op (E.fromRight Nothing (last inner)) xs
  where
    -- inner loop
    inner = takeWhile E.isRight $ scanl go (Right pal) rng
    go = flip (palindrome op l) . either id id

palindrome ::
  (Integer -> Integer -> Bool) ->
  Integer ->
  Integer ->
  Palindrome ->
  Either Palindrome Palindrome
palindrome op left right pal
  | Mb.isJust $ M.mfilter (not . op pdt . fst) pal = Left Nothing
  | isPal (show pdt) = Right justPal
  | otherwise = Right pal
  where
    pdt = left * right
    isPal = M.ap (==) reverse
    {-
    If the newly found palindrome is not the same as the one found
    before, we need to reset the factors. One palindrome may have
    multiple factors, like 9 has [1, 9] and [3, 3].
    -}
    newFact (p, fact) = if p == pdt then fact else []
    newPal = fmap ((pdt,) . ((left, right) :) . newFact) pal
    justPal = M.msum [newPal, Just (pdt, [(left, right)])]
