module Bowling (score, BowlingError (..)) where

import qualified Data.Ix as I
import qualified Data.List as L
import qualified Data.Maybe as M

data BowlingError
  = IncompleteGame
  | InvalidRoll {rollIndex :: Int, rollValue :: Int}
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls
  | over && M.isJust et = Left $ uncurry InvalidRoll $ M.fromJust et
  | M.isJust is = Left $ uncurry InvalidRoll $ M.fromJust is
  | not over = Left IncompleteGame
  | otherwise = Right $ score' 0 f
  where
    f = frames rolls
    (over, n) = isGameOver f
    et = excessThrow n rolls
    is = invalidScore f

{-
Score the game; since any bonus points are
incorporated in the frame score, no need to
check beyond the first ten frames
-}
score' :: Int -> [[Int]] -> Int
score' _ [] = 0
score' 10 _ = 0
score' i (xs : ys) = x + bonus + score' (i + 1) ys
  where
    x = sum xs
    n = length xs
    bonus = case () of
      _
        | x == 10 && n == 1 -> sum $ take 2 $ concat ys
        | x == 10 -> head $ head ys
        | otherwise -> 0

-- Check if there are any excess throws after the
-- game is over
excessThrow :: Int -> [Int] -> Maybe (Int, Int)
excessThrow numThrows rolls
  | n > numThrows = Just (numThrows, rolls !! numThrows)
  | otherwise = Nothing
  where
    n = length rolls

-- Check if any of the scores is invalid
-- i.e. outside the range [0, 10]
invalidScore :: [[Int]] -> Maybe (Int, Int)
invalidScore f = case over of
  Just (i, x : y : _) -> Just (numThrows i - 1, if x < 10 then y else x)
  Just (i, x : _) -> Just (numThrows i - 1, x)
  _ -> Nothing
  where
    over = L.find (\(_, xs) -> outOfRange xs) $ zip [1 ..] f
    outOfRange xs = not $ I.inRange (0, 10) (sum xs)
    numThrows x = sum $ map length $ take x f

-- Determine if game is over, and if yes, maximum
-- number of throws that are allowed
isGameOver :: [[Int]] -> (Bool, Int)
isGameOver f
  | n >= 10 && completeInTenth = (True, 20)
  | n >= 11 && completeInEleventh = (True, 21)
  | n >= 12 = (True, 21)
  | otherwise = (False, numThrows)
  where
    n = length f
    tenthFrame = f !! 9
    tenthFrameNumThrows = length tenthFrame
    tenthFrameScore = sum tenthFrame
    completeInTenth = tenthFrameNumThrows == 2 && tenthFrameScore < 10
    eleventhFrameNumThrows = length (f !! 10)
    completeInEleventh = tenthFrameNumThrows + eleventhFrameNumThrows >= 3
    numThrows = sum $ map length $ take n f

-- Group throws into frames
frames :: [Int] -> [[Int]]
frames [] = []
frames [x] = [[x]]
frames (x : xs)
  | x == 10 = [x] : frames xs
  | otherwise = [x, head xs] : frames (tail xs)