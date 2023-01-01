module Meetup (Weekday (..), Schedule (..), meetupDay) where

import qualified Data.List as L
import qualified Data.Maybe as M
import Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as C
import qualified Data.Time.Calendar.WeekDate as WD

data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show, Bounded, Enum, Eq)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First -> head xs
  Second -> (head . tail) xs
  Third -> xs !! 2
  Fourth -> xs !! 3
  Last -> last xs
  Teenth ->
    M.fromJust $
      L.find (\d -> let (_, _, x) = C.toGregorian d in x `elem` [13 .. 19]) xs
  where
    fg = C.fromGregorian year month
    firstDay = fg 1
    lastDay = fg $ C.gregorianMonthLength year month
    -- TODO: Replace with periodAlldays (YearMonth year month) once time >= 1.12.1
    allDays = map WD.toWeekDate [firstDay .. lastDay]
    weekdays = [minBound :: Weekday ..]
    day = ((+ 1) . fromEnum . M.fromJust) $ L.find (== weekday) weekdays
    xs =
      map (\(y, w, d) -> WD.fromWeekDate y w d) $
        filter (\(_, _, d) -> d == day) allDays
