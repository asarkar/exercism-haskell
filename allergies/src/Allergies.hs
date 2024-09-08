module Allergies (Allergen (..), allergies, isAllergicTo) where

import Data.Bits ((.&.))
import qualified Data.Bits as B
import Data.List ((!!))
import qualified Data.List as L
import qualified Data.Maybe as M

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Show, Bounded, Enum, Eq)

allergies :: Int -> [Allergen]
allergies score = map (allergens !!) ordinals
  where
    allergens = [minBound :: Allergen ..]
    -- If the ith bit is set, the result is that 2^i (greater than 0)
    ordinals = [i | i <- [0 .. length allergens - 1], score .&. (B.bit i) > 0]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
