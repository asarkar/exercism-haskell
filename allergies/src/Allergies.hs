module Allergies (Allergen (..), allergies, isAllergicTo) where

import Data.Bits ((.&.))
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
allergies score = case subsum effectiveScore scores of
  Just xs -> map (fst . M.fromJust . findAllergenWithScore) xs
  _ -> []
  where
    -- limit score to 255
    effectiveScore = score .&. 255
    intPow x = 2 ^ (x :: Int)
    allergens = [minBound :: Allergen ..]
    scoreMapping = zipWith (\x y -> (x, intPow y)) allergens [0 ..]
    scores = map snd scoreMapping
    findAllergenWithScore x = L.find ((== x) . snd) scoreMapping

{-
At each iteration, prepend the current number to each sublist.
Merge the previous and new lists.

Items are iterated in reverse to avoid costly list append.
Example: items = [1, 2, 3]

prev=[(0,[])]            new=[(3,[3])]
prev=[(0,[]),(3,[3])]    new=[(2,[2]),(5,[2,3])]
prev=[(0,[]),(2,[2]),(3,[3]),(5,[2,3])]
new=[(1,[1]),(3,[1,2]),(4,[1,3]),(6,[1,2,3])]

Final: [
         (0,[]),(1,[1]),(2,[2]),(3,[3]),
         (4,[1,3]),(5,[2,3]),(6,[1,2,3])
       ]

When two subsets have the same sum, pick one, doesn't matter which.
-}
subsum :: Int -> [Int] -> Maybe [Int]
subsum score items = (fmap snd . L.find ((== score) . fst)) combinations
  where
    combinations = foldl s [(0, [])] $ reverse items
    s acc x = merge acc $ map append acc
      where
        append (sm, l) = (sm + x, x : l)

    -- Keep list of sums sorted and unique.
    merge [] a = a
    merge a [] = a
    merge a@(x@(av, _) : xxs) b@(y@(bv, _) : yys)
      | av < bv = x : merge xxs b
      | av == bv = x : merge xxs yys
      | otherwise = y : merge a yys

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
