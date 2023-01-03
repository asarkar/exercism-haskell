module Proverb (recite) where

import qualified Data.List as L
import qualified Text.Printf as P

-- Good functional solution
-- https://exercism.org/tracks/haskell/exercises/proverb/solutions/Acaccia
recite :: [String] -> String
recite nouns = L.intercalate "\n" $ ys ++ [lastLine | (not . null) nouns]
  where
    xs = zip nouns $ tail nouns
    lineFmt = "For want of a %s the %s was lost."
    lastLine = P.printf "And all for the want of a %s." $ head nouns
    ys = map (uncurry (P.printf lineFmt)) xs
