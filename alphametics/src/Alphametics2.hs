{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Alphametics2 (solve) where

import qualified Control.Monad as M
import qualified Control.Monad.Extra as E
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as TS
import qualified Data.Char as C
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- alternative implementation using State Monad
solve :: String -> Maybe [(Char, Int)]
solve puzzle
  | any (\x -> length x > (length . head) res) eqn = Nothing
  -- solution here is the auto-generated accessor function
  | otherwise = (findSoln . solution) $ do
      let nonZeroLetters = S.fromList nz
      let equation = (V.fromList . map (VU.fromList . reverse)) eqn
      let result = (VU.fromList . reverse . head) res
      -- solution here is the Vector for letter-digit mapping
      let solution = VU.replicate 26 (-1)
      TS.execState (canSolve 0 0 0) PuzzleState {..}
  where
    xs = filter (all C.isAsciiUpper) $ words puzzle
    (eqn, res) = L.splitAt (length xs - 1) xs
    nz = [head x | x <- xs, length x > 1]
    chr x = C.chr (C.ord 'A' + x)
    findSoln v = case [ (chr x, y)
                      | x <- [0 .. 25],
                        let y = v VU.! x,
                        y >= 0
                      ] of
      [] -> Nothing
      x -> Just x

data PuzzleState = PuzzleState
  { equation :: V.Vector (VU.Vector Char),
    result :: VU.Vector Char,
    nonZeroLetters :: Set Char,
    solution :: VU.Vector Int
  }

canSolve :: Int -> Int -> Int -> State PuzzleState Bool
canSolve row col carry = do
  PuzzleState {equation, result, nonZeroLetters, solution} <- TS.get

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
          let i = solution VU.! x
          let assigned = i >= 0
          let canBeZero = flip S.notMember nonZeroLetters
          let sumDigit = carry `mod` 10
          let used = map (solution VU.!) [0 .. 25]
          let unused =
                filter
                  (\y -> y > 0 || canBeZero letter)
                  [0 .. 9]
                  L.\\ used

          case () of
            _
              | addend && assigned -> canSolve (row + 1) col (carry + i)
              | addend -> assignAny x unused
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
                    x
                    sumDigit
  where
    ord c = C.ord c - C.ord 'A'
    assignAny _ [] = return False
    assignAny ix (i : xs) = do
      success <- assign (row + 1) col (carry + i) ix i
      if success then return success else assignAny ix xs
    assign r c cr ix i = do
      -- canSolve is called with the modified state
      success <- TS.withState (assign' ix i) (canSolve r c cr)
      M.unless success (TS.modify (assign' ix (-1)))
      return success
    assign' ix i p@PuzzleState {..} = p {solution = solution VU.// [(ix, i)]}
