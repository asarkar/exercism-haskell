module Allergies (Allergen (..), allergies, isAllergicTo) where

import qualified Data.Bits as B

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

-- If the ith bit is set, allergic to the corresponding allergen
allergies :: Int -> [Allergen]
allergies score = [allergens !! i | i <- [0 .. n], isAllergic i]
  where
    allergens = [minBound :: Allergen ..]
    n = length allergens - 1
    isAllergic = B.testBit score

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
