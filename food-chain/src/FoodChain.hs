module FoodChain (song) where

import qualified Data.List as L
import qualified Text.Printf as P

song :: String
song = L.intercalate "\n" $ map (paragraph . fst) rows

paragraph :: String -> String
paragraph animal =
  unlines $
    (firstLine : [secondLine | (not . null) secondLine]) ++ case animal of
      "horse" -> []
      _ -> middleLines ++ [lastLine]
  where
    slice _ [] = []
    slice a (y@(x, _) : xs) = y : if a /= x then slice a xs else []
    firstLine = P.printf "I know an old lady who swallowed a %s." animal
    slices = reverse $ slice animal rows
    secondLine = (snd . head) slices
    lastLine = "I don't know why she swallowed the fly. Perhaps she'll die."
    animals = zip (map fst slices) (map fst $ tail slices)
    middleLine = "She swallowed the %s to catch the %s%s%c"
    birdLine = " that wriggled and jiggled and tickled inside her"
    middleLines = map go animals
    go (x, y) = case x of
      "fly" -> (snd . head) rows
      _ ->
        let k = if x == "bird" then birdLine else ""
         in P.printf middleLine x y k '.'

rows :: [(String, String)]
rows =
  [ ("fly", ""),
    ("spider", "It wriggled and jiggled and tickled inside her."),
    ("bird", "How absurd to swallow a bird!"),
    ("cat", "Imagine that, to swallow a cat!"),
    ("dog", "What a hog, to swallow a dog!"),
    ("goat", "Just opened her throat and swallowed a goat!"),
    ("cow", "I don't know how she swallowed a cow!"),
    ("horse", "She's dead, of course!")
  ]
