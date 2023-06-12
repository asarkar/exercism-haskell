module Yacht (yacht, Category (..)) where

import qualified Control.Arrow as A
import qualified Data.List as L
import qualified Data.Map as M

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht
  deriving (Eq, Ord, Enum)

yacht :: Category -> [Int] -> Int
yacht category dice =
  let f = M.fromList . map (head A.&&& length) . L.group . L.sort $ dice
      cnt k = M.findWithDefault 0 k f
   in case category of
        n | n <= Sixes -> let x = 1 + fromEnum n in x * cnt x
        FullHouse | M.size f == 2 -> M.foldrWithKey (\k v acc -> k * v + acc) 0 $ M.filter (\x -> x == 2 || x == 3) f
        FourOfAKind -> M.foldrWithKey (\k _ _ -> k * 4) 0 $ M.filter (>= 4) f
        LittleStraight | M.size f == 5 && M.notMember 6 f -> 30
        BigStraight | M.size f == 5 && M.notMember 1 f -> 30
        Choice -> sum dice
        Yacht | M.size f == 1 -> 50
        _ -> 0
