module RotationalCipher (rotate) where

import qualified Data.Char as C

rotate :: Int -> String -> String
rotate key = map $ shift k
  where
    k = key `mod` 26

shift :: Int -> Char -> Char
shift k ch
  | C.isAsciiUpper ch = C.chr $ go 'A'
  | C.isAsciiLower ch = C.chr $ go 'a'
  | otherwise = ch
  where
    go a = C.ord a + (C.ord ch + k - C.ord a) `mod` 26
