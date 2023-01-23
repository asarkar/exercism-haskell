{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Frequency (frequency) where

import Control.Parallel.Strategies (Strategy)
import qualified Control.Parallel.Strategies as S
import qualified Data.Char as C
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

type Freq = Map Char Int

-- Can't use Control.Concurrent because there's no IO here
frequency :: Int -> [Text] -> Freq
{-
parBuffer creates a bunch of sparks and performs slightly worse
than parListChunk.
-}
frequency n texts = M.unionsWith (+) (map freq texts `S.using` chunks)
  where
    {-
    `parListChunk n` divides a list into chunks of size n, and evaluates
    all chunks in parallel.
    Each chunk is evaluated according to the given strategy (here rdeepseq).
    -}
    chunks :: Strategy [Freq]
    chunks = S.parListChunk (n * 100) S.rdeepseq
    {-
    parBuffer evaluates up to the i+n+1 elements in parallel when the ith
    element is evaluated to Weak Head Normal Form.
    In other words, consuming the head element triggers parallel evaluation
    of elements up to the n+1 element, consuming the next element triggers
    evaluation of the n+2 element, and so on.
    -}
    buffers :: Strategy [Freq]
    buffers = S.parBuffer n S.rdeepseq
    freq :: Text -> Freq
    -- strict fold
    freq = T.foldl' acc M.empty . T.toLower
      where
        acc m c
          | C.isLetter c = M.insertWith (+) c 1 m
          | otherwise = m
