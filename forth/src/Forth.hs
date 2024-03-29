{-# LANGUAGE RecordWildCards #-}

module Forth
  ( ForthError (..),
    ForthState,
    evalText,
    toList,
    emptyState,
  )
where

import qualified BuiltIn as BI
import qualified Control.Monad as M
import qualified Data.Bifunctor as BF
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Defn as D
import Grammar
  ( AWord (..),
    Cmd (BuiltIn),
    LineItem (Cmd, WordDefn),
  )
import qualified Grammar as G
import Types

emptyState :: ForthState
emptyState = ForthState [] Map.empty 0

evalText :: Text -> ForthState -> ForthResult
evalText text = case G.parseLine text of
  Right items -> flip (M.foldM (flip eval)) items
  Left _ -> const $ Left InvalidWord

eval :: LineItem -> ForthState -> ForthResult
eval item = case item of
  Cmd cmd -> runCmd cmd
  WordDefn w defn -> D.newDefn w defn

runCmd :: Cmd -> ForthState -> ForthResult
runCmd cmd = case cmd of
  BuiltIn b -> BI.runBuiltIn b
  G.Word w -> runWord w

runWord :: AWord -> ForthState -> ForthResult
runWord w st = case w of
  BinOp op -> fM $ T.pack $ show op
  Other txt -> fM txt
  where
    fM txt = M.foldM f st (D.resolveWord txt st)
    f st' x = bi x >>= flip BI.runBuiltIn st'
    bi txt = BF.first (const (UnknownWord txt)) (G.parseBuiltIn txt)

toList :: ForthState -> [Int]
toList ForthState {..} = reverse stack
