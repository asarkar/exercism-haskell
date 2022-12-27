module Beer (song) where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Text.Printf as T

song :: String
song = L.intercalate "\n" $ map verse [99, 98 .. 0]

numBottles :: Int -> String
numBottles n = case n of
  0 -> "No more bottles"
  1 -> "1 bottle"
  _ -> T.printf "%d bottles" n

numBottlesLeft :: Int -> String
numBottlesLeft n = case n of
  0 -> "99 bottles"
  1 -> "no more bottles"
  _ -> numBottles (n - 1)

beginningOf2ndLine :: Int -> String
beginningOf2ndLine n = case n of
  0 -> "Go to the store and buy some more"
  1 -> "Take it down and pass it around"
  _ -> "Take one down and pass it around"

verse :: Int -> String
verse n = T.printf "%s%s" firstLine secondLine
  where
    x = numBottles n
    firstLine =
      T.printf
        "%s of beer on the wall, %s of beer.\n"
        x
        (map C.toLower x) ::
        String
    secondLine =
      T.printf
        "%s, %s of beer on the wall.\n"
        (beginningOf2ndLine n)
        (numBottlesLeft n) ::
        String