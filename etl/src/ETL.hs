module ETL (transform) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M

transform :: M.Map a String -> M.Map Char a
transform legacyData = M.fromList ys
  where
    xs = M.toList legacyData
    go acc (k, v) = acc ++ map (\x -> (C.toLower x, k)) v
    ys = L.foldl go [] xs
