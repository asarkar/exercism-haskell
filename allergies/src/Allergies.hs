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
{-
Step-by-step breakdown of converting to a pointfree expression

isAllergicTo allergen score = allergen `elem` allergies score

1. Rewrite to remove the infix

isAllergicTo allergen score = elem allergen (allergies score)

2. Change nested function application to composition

isAllergicTo allergen score = (elem allergen . allergies) score

3. eta-reduce

isAllergicTo allergen = elem allergen . allergies

4. Rewrite to use a section. The rule used here is:
(# y) = \x -> x # y

See this StackOverflow post for more explanation on Sections.
https://stackoverflow.com/q/57019292/839733

Also see Programming in Haskell by Graham Hutton, section 4.6.

isAllergicTo allergen = (. allergies) (elem allergen)

5. Change nested function application to composition

isAllergicTo allergen = ((. allergies) . elem) allergen

6. eta-reduce

isAllergicTo = (. allergies) . elem
-}
isAllergicTo = (. allergies) . elem
