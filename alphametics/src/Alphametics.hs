{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Alphametics (solve) where

import qualified Control.Monad as M
import qualified Control.Monad.Extra as E
import Control.Monad.ST (ST)
import qualified Control.Monad.Trans.Class as TC
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Char as C
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UM

-- implementation using ST Monad
solve :: String -> Maybe [(Char, Int)]
solve puzzle
  {-
  validate equation, "ABC + DEF == GH" is invalid,
  sum isn't wide enough
  -}
  | any (\x -> length x > (length . head) res) eqn = Nothing
  | otherwise = findSoln $ VU.create $ do
      let nonZeroLetters = S.fromList nz
      -- process in reverse
      let equation = (V.fromList . map (VU.fromList . reverse)) eqn
      let result = (VU.fromList . reverse . head) res
      solution <- UM.replicate 26 (-1)
      _ <- R.runReaderT (canSolve 0 0 0) PuzzleState {..}
      return solution
  where
    xs = filter (all C.isAsciiUpper) $ words puzzle
    (eqn, res) = L.splitAt (length xs - 1) xs
    -- leading letters can't be zero
    nz = [head x | x <- xs, length x > 1]
    chr x = C.chr (C.ord 'A' + x)
    findSoln v = case [ (chr x, y)
                        | x <- [0 .. 25],
                          let y = v VU.! x,
                          y >= 0
                      ] of
      [] -> Nothing
      x -> Just x

data PuzzleState s = PuzzleState
  { equation :: V.Vector (VU.Vector Char),
    result :: VU.Vector Char,
    nonZeroLetters :: Set Char,
    solution :: MVector s Int
  }

type M s = ReaderT (PuzzleState s) (ST s)

{-
If we are beyond the leftmost digit of the sum:
  Return true if no carry, false otherwise.
  Also check that there is no leading zero in the sum.
Else if addend and current column index is beyond the current row:
  Recur on row beneath this one.

If we are currently trying to assign a char in one of the addends:
  If char already assigned, recur on row beneath this one.
  If not assigned, then:
    For every possible choice among the digits not in use:
      Make that choice and recur on row beneath this one.
        If successful, return true.
        Else, unmake assignment and try another digit.
    Return false if no assignment worked to trigger backtracking.

Else if trying to assign a char in the sum:
  If char already assigned:
    If matches the sum digit, recur on next column to the left with carry.
    Else, return false to trigger backtracking.
  If char unassigned:
    If correct digit already used, return false.
    Else:
      Assign it and recur on next column to the left with carry:
        If successful return true.
        Else, unmake assignment, and return false to trigger backtracking.
-}
canSolve :: Int -> Int -> Int -> M s Bool
canSolve row col carry = do
  PuzzleState {equation, result, nonZeroLetters, solution} <- R.ask

  let addend = row < V.length equation
  let word = if addend then equation V.! row else result
  let n = VU.length word

  case () of
    _
      | col >= n && addend -> canSolve (row + 1) col carry
      | col == n && not addend -> return $ carry == 0
      | otherwise -> do
          let letter = word VU.! col
          let x = ord letter
          i <- readM solution x
          let assigned = i >= 0
          let canBeZero = flip S.notMember nonZeroLetters
          let sumDigit = carry `mod` 10
          used <- M.mapM (readM solution) [0 .. 25]
          let unused =
                filter
                  (\y -> y > 0 || canBeZero letter)
                  [0 .. 9]
                  L.\\ used

          case () of
            _
              | addend && assigned -> canSolve (row + 1) col (carry + i)
              | addend -> assignAny solution x unused
              | assigned ->
                  pure (sumDigit == i)
                    E.&&^ canSolve 0 (col + 1) (carry `div` 10)
              | sumDigit `elem` used -> return False
              | sumDigit == 0 && (not . canBeZero) letter -> return False
              | otherwise ->
                  assign
                    0
                    (col + 1)
                    (carry `div` 10)
                    solution
                    x
                    sumDigit
  where
    {-
    lift is needed because we're working in in a ReaderT monad,
    whereas VM.read and VM.write work in the ST monad
    -}
    readM solution = TC.lift . UM.read solution
    ord c = C.ord c - C.ord 'A'
    assignAny _ _ [] = return False
    assignAny solution ix (i : xs) = do
      success <- assign (row + 1) col (carry + i) solution ix i
      if success then return success else assignAny solution ix xs
    assign r c cr solution ix i = do
      -- no need to lift write since the return type isn't used
      UM.write solution ix i
      success <- canSolve r c cr
      M.unless success (UM.write solution ix (-1))
      return success
