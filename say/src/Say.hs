{-# LANGUAGE NumericUnderscores #-}

module Say (inEnglish) where

import qualified Data.List as L

inEnglish :: Integer -> Maybe String
inEnglish 0 = Just "zero"
inEnglish n = inEnglish' n

inEnglish' :: Integer -> Maybe String
inEnglish' n = case closest n of
  Just (x, y) -> do
    let (a, b) = divMod n x
    lft <- inEnglish' a
    rt <- inEnglish' b
    return (unwords $ [lft, y] ++ [rt | b > 0])
  -- n < 100
  _ -> case exact n of
    Just (_, y) -> Just y
    _ -> do
      let (a, b) = (n `div` 10 * 10, n `mod` 10)
      lft <- fmap snd (exact a)
      rt <- fmap snd (exact b)
      return (lft ++ "-" ++ rt)

exact :: Integer -> Maybe (Integer, String)
exact n = L.find ((== n) . fst) xs
  where
    xs =
      [ (0, ""),
        (1, "one"),
        (2, "two"),
        (3, "three"),
        (4, "four"),
        (5, "five"),
        (6, "six"),
        (7, "seven"),
        (8, "eight"),
        (9, "nine"),
        (10, "ten"),
        (11, "eleven"),
        (12, "twelve"),
        (13, "thirteen"),
        (14, "fourteen"),
        (15, "fifteen"),
        (16, "sixteen"),
        (17, "seventeen"),
        (18, "eighteen"),
        (19, "nineteen"),
        (20, "twenty"),
        (30, "thirty"),
        (40, "forty"),
        (50, "fifty"),
        (60, "sixty"),
        (70, "seventy"),
        (80, "eighty"),
        (90, "ninety")
      ]

{-
Find the number with the greatest number of zeros less than or equal to n.
Example: For 120, this will find 100.
-}
closest :: Integer -> Maybe (Integer, String)
closest n = L.find ((<= n) . fst) xs
  where
    xs =
      [ (1_000_000_000_000, "trillion"),
        (1_000_000_000, "billion"),
        (1_000_000, "million"),
        (1_000, "thousand"),
        (100, "hundred")
      ]
