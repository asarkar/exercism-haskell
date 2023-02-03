{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Connect (Mark (..), winner) where

import qualified Control.Monad as M
import qualified Control.Monad.Trans.Class as TC
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.Maybe as TMb
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as TS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner brd = case M.msum results of
  Just 'X' -> Just Cross
  Just 'O' -> Just Nought
  _ -> Nothing
  where
    xInit = initialState brd 'X'
    oInit = initialState brd 'O'
    n = V.length $ board xInit
    m = maybe 0 VU.length (board xInit V.!? 0)
    go b ch (row, col) = (b V.! row) VU.! col == ch
    xStarts = (filter (go (board xInit) 'X') . map (,0)) [0 .. n - 1]
    oStarts = (filter (go (board oInit) 'O') . map (0,)) [0 .. m - 1]
    xGoal (_, col) = col == m - 1
    oGoal (row, _) = row == n - 1
    results =
      map (search xInit xGoal) xStarts
        ++ map (search oInit oGoal) oStarts
    search p@PgmSt {..} goal s = TS.evalState (TMb.runMaybeT (bfs goal)) st
      where
        st = p {queue = queue Seq.|> s}

type Cell = (Int, Int)

type Grid = V.Vector (VU.Vector Char)

data PgmSt = PgmSt
  { visited :: Set Cell,
    queue :: Seq Cell,
    board :: Grid
  }
  deriving (Show)

initialState :: [String] -> Char -> PgmSt
initialState brd ch =
  PgmSt
    { visited = Set.empty,
      queue = Seq.empty,
      board = board
    }
  where
    board = V.fromList $ map (VU.fromList . map go . words) brd
    go xs
      | head xs == ch = ch
      | otherwise = '.'

next :: Cell -> Set Cell -> V.Vector (VU.Vector Char) -> [Cell]
next (row, col) visited board = filter isValid candidates
  where
    candidates =
      [ (row - 1, col),
        (row - 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col - 1),
        (row + 1, col)
      ]
    n = V.length board
    m = (VU.length . V.head) board
    isInRng (r, c) = r >= 0 && r < n && c >= 0 && c < m
    isNotVisited x = Set.notMember x visited
    isValidChar (r, c) = '.' /= (board V.! r) VU.! c
    {-
    `(->) r` is a monad, so if we specialize the `m` in sequence we get:
      sequence :: t ((->) r a) -> (->) r (t a)

      More about `(->) r` is here:
      https://www.reddit.com/r/haskell/comments/10oqew8/comment/j6gace2/?utm_source=share&utm_medium=web2x&context=3

      `t` is `[]` so we have:
      sequence :: [r -> a] -> r -> [a]

      `r` is whatever `x` is (Cell) and `a` is `Bool` so we get:
      sequence :: [Cell -> Bool] -> Cell -> [Bool]

      we can then compose this with `and :: [Bool] -> Bool`
    -}
    isValid = and . sequence [isInRng, isNotVisited, isValidChar]

bfs :: (Cell -> Bool) -> MaybeT (State PgmSt) Char
bfs isGoal = do
  {-
  lift is needed because we are operating in the `MaybeT` context,
  not `State` context. 'mtl' can help avoid this.
  -}
  PgmSt {visited, queue, board} <- TC.lift TS.get
  M.guard ((not . Seq.null) queue)

  let (cell@(row, col) Seq.:<| rest) = queue

  case isGoal cell of
    True -> return ((board V.! row) VU.! col)
    False | Set.member cell visited -> do
      TC.lift $ TS.modify (replaceQ rest)
      bfs isGoal
    _ -> do
      TC.lift $ TS.modify (visit cell)
      let neighbors = next cell visited board
      TC.lift $ TS.modify (replaceQ (foldl (Seq.|>) rest neighbors))

      bfs isGoal
  where
    visit cell p@PgmSt {..} = p {visited = Set.insert cell visited}
    replaceQ q p@PgmSt {} = p {queue = q}
