module House (rhyme) where

import qualified Data.List as L

drTillThe :: String -> String
drTillThe ('t' : 'h' : 'e' : xs) = xs
drTillThe x = drTillThe (tail x)

chFirst :: [String] -> [String]
chFirst [] = []
chFirst (x : xs) = "" : ("This is the" ++ drTillThe x) : xs

lasts :: String
lasts =
  "that the horse and the hound and the horn\n\
  \that belonged to the farmer sowing his corn\n\
  \that kept the rooster that crowed in the morn\n\
  \that woke the priest all shaven and shorn\n\
  \that married the man all tattered and torn\n\
  \that kissed the maiden all forlorn\n\
  \that milked the cow with the crumpled horn\n\
  \that tossed the dog\n\
  \that worried the cat\n\
  \that killed the rat\n\
  \that ate the malt\n\
  \that lay in the house that Jack built.\n"

-- Copied from https://exercism.org/tracks/haskell/exercises/house/solutions/razetime
rhyme :: String
rhyme = unlines (tail (concat (reverse (map chFirst (L.tails $ lines lasts)))))
