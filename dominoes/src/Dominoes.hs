{-# LANGUAGE RecordWildCards #-}

module Dominoes (chain) where

import qualified Control.Monad.Extra as ME
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as TS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

type Domino = (Int, Int)

data PgmSt = PgmSt
  { result :: [Domino],
    idxMap :: IntMap (Set Int),
    dominoes :: Vector Domino,
    n :: Int
  }
  deriving (Show)

{-
The problem can be rephrased to say that given some pairs/tuples,
arrange them such that the adjacent sides have the same number.

You might think that given [(1, 1), (2, 2)], a possible answer is
[(1, 2), (2, 1)], but no, because the pairs (1, 2) and (2, 1)
don't exist in the input. However, given [(1, 2) and (1, 2)],
[(1, 2), (2, 1)] is acceptable because we simply flipped the
tuple (2, 1).

Thus, the problem reduces to finding tuples with either the left
or the right matching a given number, and trying them one by one
until a complete chain of dominoes is formed.

We keep a Map of left and right values as the keys, and the
corresponding domino indices as the values. According to the usual
backtracking paradigm, we need to acquire and release an index,
so we provide methods to do so.
-}
chain :: [Domino] -> Maybe [Domino]
chain [] = Just []
chain dominoes = if solved then Just res else Nothing
  where
    {-
    Due to the symmetry in the problem, it doesn't matter which
    domino or side we start from.
    -}
    (solved, st) = TS.runState (solve False 0) $ initialState dominoes
    res = result st

initialState :: [Domino] -> PgmSt
initialState xs = PgmSt {result = [], ..}
  where
    n = length xs
    dominoes = V.fromList xs
    idxMap = foldr go IM.empty [0 .. n - 1]
    go i mp = foldr (\k acc -> IM.insertWith Set.union k v acc) mp [l, r]
      where
        (l, r) = dominoes V.! i
        v = Set.singleton i

updateIdx :: (Int -> Set Int -> Set Int) -> Int -> State PgmSt ()
updateIdx f i = do
  dom <- TS.gets dominoes
  let (l, r) = dom V.! i
  TS.modify $ \p@PgmSt {..} ->
    p {idxMap = foldr (IM.adjust (f i)) idxMap [l, r]}

acquire :: Int -> State PgmSt ()
acquire = updateIdx Set.delete

release :: Int -> State PgmSt ()
release = updateIdx Set.insert

{-
1. Should return all indices that have either left
   or right matching the the given domino side.
2. Should not contain the given domino.
3. Should not contain duplicate indices.
4. Should contain identical dominoes with unique indices.
-}
candidates :: Int -> Bool -> State PgmSt [Int]
candidates i left = do
  dom <- TS.gets dominoes
  idx <- TS.gets idxMap
  let (l, r) = dom V.! i
  let side = if left then l else r
  return (select $ idx IM.! side)
  where
    select = Set.toList . Set.filter (/= i)

solve :: Bool -> Int -> State PgmSt Bool
-- left = true indicates candidates should be matched by the left value
solve left i = do
  dom <- TS.gets dominoes
  let (l, r) = dom V.! i

  {-
  Since we append new dominoes at the beginning,
  if matched on the right side, this tuple has
  to be flipped.
  -}
  TS.modify $ push $ if left then (l, r) else (r, l)
  acquire i

  cn <- candidates i left
  solved <-
    ME.anyM
      ( \j ->
          let r' = snd (dom V.! j)
           in solve (r == r' || l == r') j
      )
      cn

  if solved
    then return True
    else do
      release i
      res <- TS.gets result
      n' <- TS.gets n
      if length res == n'
        then return $ isComplete res
        else do
          TS.modify pop
          return False
  where
    push dom p@PgmSt {..} = p {result = dom : result}
    pop p@PgmSt {..} = p {result = tail result}
    isComplete res = (fst . head) res == (snd . last) res
