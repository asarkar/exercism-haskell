module RotationalCipher (rotate) where

import qualified Data.Char as C

rotate :: Int -> String -> String
rotate key = map (C.chr . shift k)
  where
    k =
      key `mod` numLetters
        + ( if key < 0
              then numLetters
              else 0
          )

numLetters :: Int
numLetters = 26

shift :: Int -> Char -> Int
shift k ch
  | C.isAsciiUpper ch && x > C.ord 'Z' = x - numLetters
  | C.isAsciiLower ch && x > C.ord 'z' = x - numLetters
  | C.isAlpha ch = x
  | otherwise = C.ord ch
  where
    x = k + C.ord ch
