module TwelveDays (recite) where

import qualified Data.Vector as V
import qualified Text.Printf as T

recite :: Int -> Int -> [String]
recite start stop = map recite' [start .. stop]

recite' :: Int -> String
recite' i = T.printf prefix s rest
  where
    prefix = "On the %s day of Christmas my true love gave to me:%s a Partridge in a Pear Tree."
    s = fst $ (V.!) lyrics (i - 1)
    rest =
      let xs = (V.reverse . V.map snd) (V.slice 1 (i - 1) lyrics)
          go = T.printf "%s%s, "
       in case i of
            1 -> ""
            _ -> T.printf "%sand" (V.foldl go " " xs)
    lyrics =
      V.fromList
        [ ("first", "a Partridge in a Pear Tree"),
          ("second", "two Turtle Doves"),
          ("third", "three French Hens"),
          ("fourth", "four Calling Birds"),
          ("fifth", "five Gold Rings"),
          ("sixth", "six Geese-a-Laying"),
          ("seventh", "seven Swans-a-Swimming"),
          ("eighth", "eight Maids-a-Milking"),
          ("ninth", "nine Ladies Dancing"),
          ("tenth", "ten Lords-a-Leaping"),
          ("eleventh", "eleven Pipers Piping"),
          ("twelfth", "twelve Drummers Drumming")
        ]