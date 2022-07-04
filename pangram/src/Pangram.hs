module Pangram (isPangram) where

import Data.Char (toLower, isAscii, isAlpha)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = length y == 26
  where x = [toLower c | c <- text, isAscii c, isAlpha c]
        y = nub x
