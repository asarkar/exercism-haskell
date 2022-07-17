module Acronym (abbreviate) where

import qualified Data.Char as C

{-
1. If a character is the start of a word, take it. Words are separated by whitespaces.
2. If a character is preceded by a hyphen, take it.
3. If a character is upper case and not preceded by another upper case character, take it.
-}
abbreviate :: String -> String
abbreviate [] = []
abbreviate xs = [C.toUpper $ snd x | x <- ys, isFirst x || isUpper x]
  where
    ys = (' ', head xs) : zip xs (tail xs)
    isFirst (prev, this) = C.isAlpha this && (C.isSpace prev || prev == '-')
    isUpper (prev, this) = C.isUpper this && (not . C.isUpper) prev