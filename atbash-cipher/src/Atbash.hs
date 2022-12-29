module Atbash (decode, encode) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as M

decode :: String -> String
decode = xcode

encode :: String -> String
encode = unwords . chunks 5 . xcode
  where
    chunks _ [] = []
    chunks n xs = takeWhile (not . null) $ L.unfoldr (Just . splitAt n) xs

xcode :: String -> String
xcode = M.mapMaybe (go . C.toLower)
  where
    go ch
      | C.isLower ch = Just $ C.chr (C.ord 'a' + (C.ord 'z' - C.ord ch))
      | C.isDigit ch = Just ch
      | otherwise = Nothing
