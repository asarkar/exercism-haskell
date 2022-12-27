module Robot
  ( Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
  )
where

import qualified Data.List as L
import qualified Data.Maybe as M

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

type Coordinates = (Integer, Integer)

data Robot = Robot Bearing Coordinates

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> Coordinates
coordinates (Robot _ c) = c

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move = foldl f
  where
    f r ins = if ins == 'A' then advance r else turn r ins

advance :: Robot -> Robot
advance (Robot b (x, y)) = mkRobot b c
  where
    c = case b of
      North -> (x, y + 1)
      East -> (x + 1, y)
      South -> (x, y - 1)
      West -> (x - 1, y)

turn :: Robot -> Char -> Robot
turn (Robot b c) d = mkRobot b' c
  where
    bearings = [minBound :: Bearing ..]
    i = M.fromJust $ L.elemIndex b bearings
    b' = case d of
      'L' -> if i >= 1 then bearings !! (i - 1) else West
      _ -> bearings !! ((i + 1) `mod` 4)
