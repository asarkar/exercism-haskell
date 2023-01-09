module Poker (bestHands) where

import Control.Arrow ((&&&))
import qualified Control.Monad as M
import qualified Data.Function as F
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
import qualified Text.Read as R

data PokerHand
  = HighCard [Int]
  | OnePair [Int]
  | TwoPair [Int]
  | ThreeOfAKind [Int]
  | Straight [Int]
  | Flush [Int]
  | FullHouse [Int]
  | FourOfAKind [Int]
  | StraightFlush [Int]
  deriving (Show, Eq, Ord)

bestHands :: [String] -> Maybe [String]
bestHands xs
  | length pokerHands /= length hands = Nothing
  | otherwise = Just $ findWinners sortedHands
  where
    pokerHands = map pokerHand xs
    hands = zip [0 ..] $ Mb.catMaybes pokerHands
    -- desc sort, swap the two arguments to 'compare'
    -- https://ro-che.info/articles/2016-04-02-descending-sort-haskell
    sortedHands = L.sortBy (flip compare `F.on` snd) hands
    findBest = (== (snd . head) sortedHands) . snd
    findWinners = map ((xs !!) . fst) . filter findBest

pokerHand :: String -> Maybe PokerHand
pokerHand hand = flip fmap (rankMap hand) $ \ranks ->
  {-
   Sort ranks in desc order by the length of their groups,
   and for equal-length groups, break tie using the key (rank).
   A rank group consists of the suites for the same rank.
  -}
  let sortedRanks = L.sortOn Down . map ((length . snd) &&& fst) . M.assocs
      (rankGrpLen, ranks') = L.unzip $ sortedRanks ranks
      -- Count all the unique suites
      numSuites = S.size $ foldl S.union S.empty $ M.elems ranks
      numRanks = length ranks'
   in case (numSuites, numRanks) of
        -- Five different ranks
        (x, 5) ->
          let fiveHigh = ranks' == [14, 5, 4, 3, 2]
              lastRank = last ranks'
              inRng i = i >= lastRank && i <= lastRank + 4
              -- Check if the ranks are sequential
              isSeq = fiveHigh || all inRng ranks'
           in -- Match number of suites
              case () of
                _
                  | isSeq && x == 1 -> StraightFlush ranks'
                  | x == 1 -> Flush ranks'
                  -- There's a test that says a 5-high straight
                  -- is the lowest-scoring straight
                  | isSeq && x > 1 && fiveHigh -> Straight [0 .. 4]
                  | isSeq && x > 1 -> Straight ranks'
                  | otherwise -> HighCard ranks'
        -- Two different ranks
        (_, 2) -> case rankGrpLen of
          [4, 1] -> FourOfAKind ranks'
          [3, 2] -> FullHouse ranks'
          _ -> error "numRanks=2"
        -- Three different ranks
        (_, 3) -> case rankGrpLen of
          [3, 1, 1] -> ThreeOfAKind ranks'
          [2, 2, 1] -> TwoPair ranks'
          _ -> error "numRanks=3"
        -- Four different ranks
        (_, 4) -> OnePair ranks'
        (_, _) -> HighCard ranks'

-- Return a map of rank -> suites
rankMap :: String -> Maybe (Map Int (Set Char))
rankMap = M.foldM insM M.empty . (map parse . words)
  where
    insM m = fmap (\(k, v) -> M.insertWith S.union k (S.singleton v) m)

    parse xs = M.guard (validRank && validSuite) >> Just (rank, suite)
      where
        (r, s) = splitAt (length xs - 1) xs
        validSuite = suite `elem` ['S', 'H', 'D', 'C']
        validRank = rank >= 1 && rank <= 14
        suite = head s
        rank = case r of
          "A" -> 14
          "K" -> 13
          "Q" -> 12
          "J" -> 11
          _ -> Mb.fromMaybe 0 (R.readMaybe r :: Maybe Int)
