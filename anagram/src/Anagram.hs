module Anagram (anagramsFor) where

import qualified Data.Char as C
import qualified Data.List as L

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [x | x <- xss, let s = toU x, s /= ys && L.sort s == zs]
  where
    toU = map C.toUpper
    ys = toU xs
    zs = L.sort ys
