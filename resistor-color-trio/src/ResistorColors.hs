module ResistorColors (Color (..), Resistor (..), label, ohms) where

import qualified Data.List as L
import qualified Text.Printf as T

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor {bands :: (Color, Color, Color)}
  deriving (Show)

label :: Resistor -> String
label r = unwords [fmt z, unit]
  where
    x = ohms r
    -- Find the smallest unit greater than x
    y = L.find (\(_, u) -> fst u > x) (zip [0 ..] units)
    z = divBy x divisor
    (divisor, unit) = case y of
      Just (0, _) -> head units
      Just (i, _) -> units !! (i - 1)
      _ -> last units

units :: [(Int, String)]
-- Specify type otherwise error:
-- Defaulting the following constraints to type 'Integer'
units = [(1, "ohms"), (10 ^ (3 :: Int), "kiloohms"), (10 ^ (6 :: Int), "megaohms"), (10 ^ (9 :: Int), "gigaohms")]

divBy :: Int -> Int -> Float
divBy x y = fromIntegral x / fromIntegral y

fmt :: Float -> String
fmt x = if int then show y else T.printf "%.1f" x
  where
    y = round x :: Int
    int = abs (x - fromIntegral y) < 0.1

ohms :: Resistor -> Int
ohms Resistor {bands = (x, y, z)} = (a * 10 + b) * 10 ^ c
  where
    a = fromEnum x
    b = fromEnum y
    c = fromEnum z