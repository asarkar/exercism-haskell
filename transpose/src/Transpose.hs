module Transpose (transpose) where

import qualified Data.List as L
import qualified Data.Maybe as M

{-
At each iteration, we take the first character of each string
and form a column. The catch is that when a string in the
middle is shorter than any of the subsequent strings, a space
is to be inserted in the missing position to maintain continuity.
However, when the last string is shorter, then no space should
be inserted.

To achieve this, we do the following:
1. Remove all the empty strings from the end of the list.
2. When we encounter an empty string, we know for sure it's
   not at the end, so, return a space for the current character.
-}
transpose :: [String] -> [String]
transpose xs
  | null ys = []
  | otherwise = firstCol : transpose rest
  where
    ys = L.dropWhileEnd null xs
    (firstCol, rest) = unzip $ map go ys
    go = M.fromMaybe (' ', "") . L.uncons
