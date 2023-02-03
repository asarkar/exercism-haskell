{-# LANGUAGE RecordWildCards #-}

module Defn (newDefn, resolveWord) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Grammar
  ( AWord (..),
  )
import Types

isUserDefined :: Text -> ForthState -> Bool
isUserDefined txt ForthState {..} = Map.member (T.toUpper txt) defnDict

{-
Insert the definition as raw text, no need to eagerly evaluate.
Since a word may later be redefined, we store the definitions
with monotonically increasing ids, thus establishing a
happens-before relationship between any two definitions.
-}
newDefn :: AWord -> Text -> ForthState -> ForthResult
newDefn w defn st@ForthState {..} =
  Right $
    st
      { defnDict = Map.insertWith merge w' [(i, T.toUpper defn)] defnDict,
        defnId = i
      }
  where
    i = defnId + 1
    merge new old = head new : old
    w' = case w of
      Other xs -> T.toUpper xs
      BinOp op -> T.pack $ show op

resolveWord :: Text -> ForthState -> [Text]
resolveWord w st@ForthState {..}
  {-
  Don't forget to reverse the definition since we processed
  it in the reverse order.
  -}
  | isUserDefined w st = reverse $ resolve' (i + 1) st w
  | otherwise = [w]
  where
    i = fst . head $ defnDict Map.! T.toUpper w

{-
Resolve a definition by recursively replacing all user-defined words
with built-in words. If a word is not found in the dictionary, it
could be a built-in word, or an invalid one. We will find out when
we later try to parse it as a built-in word.

We process in the reverse order, or in a right-associative manner,
recursively replacing each word with its value.

For each word in the definition, we find the greatest id that is
smaller than this word's id. This gives us the definition that
existed at the time this word was defined.
-}
resolve' :: Int -> ForthState -> Text -> [Text]
resolve' i st@ForthState {..} w
  | isUserDefined w st = (concatMap (resolve' j st) . reverse . T.words) defn
  | otherwise = [w]
  where
    {-
    The '=' in '>=' avoids infinite loop for recursive definitions,
    like ': foo foo 1 ;'
    -}
    (j, defn) = head . dropWhile ((>= i) . fst) $ defnDict Map.! T.toUpper w
