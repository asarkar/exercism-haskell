module Acronym (abbreviate) where

import qualified Data.Char as C

{-
1. If a character is the start of a word, take it. Words are separated by whitespaces.
2. If a character is preceded by a hyphen, take it.
3. If a character is upper case and not preceded by another upper case character, take it.
-}
abbreviate :: String -> String
abbreviate xs = [C.toUpper (this i) | i <- [0 .. n], first i || upper i]
  where
    n = length xs - 1
    prev i = xs !! (i - 1)
    this i = xs !! i
    first i = C.isAlpha (this i) && (i == 0 || C.isSpace (prev i) || prev i == '-')
    upper i = C.isUpper (this i) && (not . C.isUpper) (prev i)