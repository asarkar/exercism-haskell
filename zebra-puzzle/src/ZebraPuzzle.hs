module ZebraPuzzle (Resident (..), Solution (..), solve) where

import qualified Control.Monad as M
import qualified Data.List as L

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Bounded, Enum, Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Bounded, Enum, Eq, Show)

data Pet = Dog | Fox | Horse | Snails | Zebra
  deriving (Bounded, Enum, Eq, Show)

data Drink = Tea | Coffee | Milk | OrangeJuice | Water
  deriving (Bounded, Enum, Eq, Show)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Bounded, Enum, Eq, Show)

data Solution = Solution
  { waterDrinker :: Resident,
    zebraOwner :: Resident
  }
  deriving (Eq, Show)

-- https://puzzling.stackexchange.com/q/78
-- https://www.youtube.com/watch?v=Qd84SEf6GbM&list=PLAwxTw4SYaPnJVtPvZZ5zXj_wRBjH0FxX&index=77
solve :: Solution
solve = Solution water zebra
  where
    ans = answers
    water = head [r | xs <- ans, (r, d, _) <- xs, d == Water]
    zebra = head [r | xs <- ans, (r, _, p) <- xs, p == Zebra]

{-
Every permutation represents an assignment of a "property"
to houses. For example, [Red, Green, Ivory, Yellow, Blue]
means house 1 is Red, house 2 is Green, etc.
We then apply the given rules to prune invalid assignments.
-}
values :: (Bounded a, Enum a) => [[a]]
values = L.permutations [minBound .. maxBound]

answers :: [[(Resident, Drink, Pet)]]
answers = do
  color <- values :: [[Color]]
  leftOf color Ivory color Green -- 6
  -- preserve break
  resident <- values :: [[Resident]]
  same resident Englishman color Red -- 2
  first resident Norwegian -- 10
  nextTo resident Norwegian color Blue -- 15
  -- preserve break
  drink <- values :: [[Drink]]
  same drink Coffee color Green -- 4
  same resident Ukrainian drink Tea -- 5
  middle drink Milk -- 9
  -- preserve break
  pet <- values :: [[Pet]]
  same resident Spaniard pet Dog -- 3
  -- preserve break
  smoke <- values :: [[Smoke]]
  same smoke OldGold pet Snails -- 7
  same smoke Kools color Yellow -- 8
  nextTo smoke Chesterfields pet Fox -- 11
  nextTo smoke Kools pet Horse -- 12
  same smoke LuckyStrike drink OrangeJuice -- 13
  same resident Japanese smoke Parliaments -- 14
  return $ zip3 resident drink pet
  where
    same xs x ys y = M.guard $ (x, y) `L.elem` zip xs ys
    leftOf xs x ys = same xs x (tail ys)
    nextTo xs x ys y =
      leftOf xs x ys y
        `M.mplus` leftOf ys y xs x
    middle xs x = M.guard $ xs L.!! 2 == x
    first xs x = M.guard $ head xs == x
