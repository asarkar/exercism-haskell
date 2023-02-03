{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import qualified Control.Applicative as A
import qualified Data.Char as C
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree (..))
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

{-
https://homepages.cwi.nl/~aeb/go/misc/sgfnotes.html
https://homepages.cwi.nl/~aeb/go/misc/sgf.html

GameTree   		= '(' Node+ GameTree* ')'
Node       		= ';' Property*
Property   		= PropId PropVal+
PropId  		= Letter+
Letter   		= 'A'..'Z'
PropVal  		= '[' Text ']'
-}

type SgfTree = Tree SgfNode

type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe SgfTree
parseSgf sgf = case M.parse (gameTree A.<* M.eof) "" sgf of
  -- :t err = M.ParseErrorBundle Text Void
  Left err ->
    let _ = putStr $ M.errorBundlePretty err
     in Nothing
  Right t -> Just t

type Parser =
  M.Parsec
    -- The type for custom error messages. We have none, so use `Void`.
    Void
    -- The input stream type. We use `Text`.
    Text

gameTree :: Parser SgfTree
gameTree = MC.char '(' *> tree <* MC.char ')'
  where
    -- the children are either another node, or a forest
    tree = Node <$> node <*> (singleNodeTree A.<|> forest)
    singleNodeTree = (\x -> [Node x []]) <$> node
    forest = M.many gameTree

node :: Parser SgfNode
node = MC.char ';' *> props
  where
    props = Map.fromList <$> M.many property

property :: Parser (Text, [Text])
property = (,) <$> propId <*> M.some propVal

propId :: Parser Text
propId = M.takeWhileP (Just "propId") C.isAsciiUpper

propVal :: Parser Text
propVal = MC.char '[' *> values <* MC.char ']'
  where
    values = T.concat <$> M.some text

{-
parse single character. there is a test that requires escaped
newline gets replaced with nothing, everything else is replaced
by usual rules.
-}
text :: Parser Text
text = escaped A.<|> other
  where
    escaped = MC.char '\\' *> (escape <$> M.anySingle)
    other = replace <$> M.anySingleBut ']'
    escape c = if c == '\n' then "" else replace c
    replace c = case c of
      '\n' -> "\n"
      '\t' -> " "
      _ -> T.singleton c
