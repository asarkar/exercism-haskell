module Isogram (isIsogram) where

import qualified Data.Char as C
import qualified Data.List as L

isIsogram :: String -> Bool
isIsogram xs = null zs
  where
    zs = filter (\x -> C.isAlpha (head x) && length x > 1) . L.group . L.sort $ ys
    ys = map C.toLower xs
