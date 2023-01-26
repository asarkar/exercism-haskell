module Change (findFewestCoins) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as S
import qualified Data.Function as F
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as MP
import qualified Data.Maybe as M

type Coins = [Integer]

type Change = Maybe Coins

type Memo = Map Integer Change

{-
We use top-down dynamic programming with memoization.
The cache is stored in a State Monad.
-}
findFewestCoins :: Integer -> Coins -> Change
findFewestCoins target coins = S.evalState (change target coins) MP.empty

change :: Integer -> Coins -> State Memo Change
-- Can make zero change with no coins
change 0 _ = return $ Just []
-- Cannot make change without any coins
change _ [] = return Nothing
change target coins = do
  memo <- S.get
  -- If in the cache, return the result
  if MP.member target memo
    then return $ MP.findWithDefault Nothing target memo
    else do
      let runS (acc, m) x = do
            let (xs, m') = S.runState (change (target - x) coins) m
            -- Functor in action
            let acc' = ((x :) <$> xs) : acc
            (acc', m')
      let xs = filter (<= target) coins
      -- Try each coin at a time
      let (ys, m) = foldl runS ([], memo) xs
      -- Discard the Nothings and unwrap the Justs
      let zs = M.catMaybes ys
      -- ((+) `on` f) x y = f x + f y
      let minBy = L.minimumBy (compare `F.on` length)
      let chng =
            if null zs
              then Nothing
              else Just $ minBy zs
      S.put $ MP.insert target chng m
      return chng
