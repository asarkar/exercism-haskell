module Anagram (anagramsFor) where

import qualified Data.List as L
import qualified Data.Char as C

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [x | x <- xss, let s = toUpper x, s /= ys && freq s == zs]
  where
    ys = toUpper xs
    zs = freq ys

toUpper :: String -> String
toUpper = map C.toUpper

freq :: String -> [(Char, Int)]
freq xs = map (\a -> (head a, length a)) $ L.group $ L.sort xs