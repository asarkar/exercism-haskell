{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Counting
  ( Color (..),
    territories,
    territoryFor,
  )
where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as TS
import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data Color = Black | White | Gray | Undetermined
  deriving (Eq, Ord, Show)

type Coord = (Int, Int)

type Board = V.Vector (VU.Vector Char)

data PgmSt = PgmSt
  { visited :: Set Coord,
    queue :: Seq Coord,
    color :: Color
  }
  deriving (Show)

{-
Basic rules of Go: https://senseis.xmp.net/?BasicRulesOfGo

Technically, a territory consists of all the cells that are
connected, i.e., it is possible to start from any one and
reach another. Thus, BFS/Union-Find can be used to find
a territory. We launch the search repeatedly from a blank
cell, and collect the results.

The 'color' of a cell is determined by the color of its
neighboring occupied cells. If there's no occupied cell
nearby, the color of this cell is yet undetermined. If
there are more than one colors of neighboring cells, then
the color if this cell is gray (not owned by either black
or white).
Note that once a color has been determined for the territory,
it's never changed, and all later cells pick up the same color.
-}

territories :: [String] -> [(Set Coord, Maybe Color)]
territories brd =
  {-
  if we launch from an already visited cell,
  we get an empty territory.
  -}
  (filter (not . null . fst) . concatMap snd) $
    scanl go (Set.empty, []) blanks
  where
    board = parseBoard brd
    n = rows board
    m = cols board
    blanks =
      filter (isBlank board)
        . concatMap (\r -> map (r,) [0 .. m - 1])
        $ [0 .. n - 1]

    {-
    we reuse the visited set to avoid exploring
    already visited cells.
    -}
    go (vs, _) start = (visited, [territory t color])
      where
        PgmSt {..} = search board (initialState vs start)
        -- we could also use a dedicated set for the territory
        t = Set.difference visited vs

territoryFor :: [String] -> Coord -> Maybe (Set Coord, Maybe Color)
territoryFor brd (col, row)
  | isInRng board (r, c) && isBlank board (r, c) =
      Just $ territory visited color
  | otherwise = Nothing
  where
    board = parseBoard brd
    r = row - 1
    c = col - 1
    PgmSt {..} = search board (initialState Set.empty (r, c))

parseBoard :: [String] -> Board
parseBoard = V.fromList . map VU.fromList

territory :: Set Coord -> Color -> (Set Coord, Maybe Color)
territory vs cl = (goCoord, cl')
  where
    goCoord = Set.map (\(r, c) -> (c + 1, r + 1)) vs
    cl' = if cl `elem` [Gray, Undetermined] then Nothing else Just cl

search :: Board -> PgmSt -> PgmSt
search board = TS.execState (bfs board)

isBlank :: Board -> Coord -> Bool
isBlank board coord = elemAt board coord == ' '

elemAt :: Board -> Coord -> Char
elemAt board (row, col) = (board V.! row) VU.! col

rows :: Board -> Int
rows = V.length

cols :: Board -> Int
cols board = maybe 0 VU.length (board V.!? 0)

isInRng :: Board -> Coord -> Bool
isInRng board (r, c) = r >= 0 && r < n && c >= 0 && c < m
  where
    n = rows board
    m = cols board

initialState :: Set Coord -> Coord -> PgmSt
initialState vs start =
  PgmSt
    { visited = vs,
      queue = Seq.singleton start,
      color = Undetermined
    }

next :: Coord -> Set Coord -> Board -> [Coord]
next (row, col) visited board = filter isValid candidates
  where
    candidates =
      [ (row - 1, col),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col)
      ]
    isNotVisited x = Set.notMember x visited
    isValid x = isInRng board x && isNotVisited x

bfs :: Board -> State PgmSt ()
bfs board = do
  PgmSt {visited, queue, color} <- TS.get
  case Seq.viewl queue of
    Seq.EmptyL -> return ()
    (coord Seq.:< rest) ->
      if Set.member coord visited
        then do
          TS.modify (setQ rest)
          bfs board
        else do
          TS.modify (visit coord)
          let (blanks, occupied) =
                L.partition (isBlank board) $ next coord visited board
          let xs = (L.nub . map color') occupied
          let cl = case xs of
                [x] | color `elem` [Undetermined, x] -> x
                [] -> color
                _ -> Gray
          TS.modify (setColor cl)
          TS.modify (setQ (foldl (Seq.|>) rest blanks))
          bfs board
  where
    color' x = case elemAt board x of
      'B' -> Black
      _ -> White
    visit coord p@PgmSt {..} = p {visited = Set.insert coord visited}
    setQ q p@PgmSt {} = p {queue = q}
    setColor cl p@PgmSt {} = p {color = cl}
