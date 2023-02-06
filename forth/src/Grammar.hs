{-# LANGUAGE OverloadedStrings #-}

module Grammar
  ( parseLine,
    parseBuiltIn,
    BinOp (..),
    StackOp (..),
    BuiltIn (..),
    AWord (..),
    Cmd (..),
    LineItem (..),
  )
where

import qualified Data.Char as C
import qualified Data.Functor as F
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL

{-
Int = @{ ASCII_DIGIT+ }
Add = { "+" }
Sub = { "-" }
Mul = { "*" }
Div = { "/" }
BinOp = { Add | Sub | Mul | Div }
Dup = { ^"DUP" }
Drop = { ^"DROP" }
Swap = { ^"SWAP" }
Over = { ^"OVER" }
StackOp = { Dup | Drop | Swap | Over }
BuiltIn = { Int | BinOp | StackOp }
COLON = _{ ":" }
SEMICOLON = _{ ";" }
WHITESPACE = _{ " " }
HyphenatedWord = _{ ASCII_ALPHA+ ~ Sub ~ ASCII_ALPHA+ }
Word = @{ HyphenatedWord | BinOp | ASCII_ALPHA+ }
Cmd = { Word | BuiltIn }
WordDefn = { COLON ~ Word ~ ANY+ ~ SEMICOLON }
InvalidWord = { COLON ~ ANY* }
LineItem = { WordDefn | Cmd | InvalidWord }
Line = _{ SOI ~ LineItem+ ~ EOI }
-}

data LineItem
  = WordDefn AWord Text
  | Cmd Cmd
  deriving (Show)

data Cmd = Word AWord | BuiltIn BuiltIn deriving (Show)

data AWord = Other Text | BinOp BinOp deriving (Show)

data BuiltIn
  = AnInt Int
  | ABinOp BinOp
  | AStackOp StackOp
  deriving (Show)

data StackOp = Dup | Drop | Swap | Over deriving (Show)

data BinOp = Add | Sub | Mul | Div

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

type Parser =
  M.Parsec
    -- The type for custom error messages. We have none, so use `Void`.
    Void
    -- The input stream type. We use `Text`.
    Text

parseLine :: Text -> Either Text [LineItem]
-- Force parser to consume entire input
-- <* Sequence actions, discarding the value of the second argument.
parseLine txt = case M.parse (line <* M.eof) "" txt of
  -- :t err = M.ParseErrorBundle String Void
  Left err -> Left . T.pack $ M.errorBundlePretty err
  Right x -> Right x

parseBuiltIn :: Text -> Either Text BuiltIn
parseBuiltIn txt = case M.parse (builtIn <* M.eof) "" txt of
  Left err -> Left . T.pack $ M.errorBundlePretty err
  Right x -> Right x

line :: Parser [LineItem]
line = lineItem `M.sepBy1` MC.space1

lineItem :: Parser LineItem
lineItem = wordDefn M.<|> (Cmd <$> cmd)

wordDefn :: Parser LineItem
wordDefn = M.between (lexeme $ MC.char ':') (MC.char ';') wd
  where
    wd = WordDefn <$> lexeme word <*> defn
    defn = T.stripEnd <$> M.takeWhile1P (Just "defn") (/= ';')

cmd :: Parser Cmd
cmd = Word <$> word M.<|> BuiltIn <$> builtIn

word :: Parser AWord
word = (BinOp <$> binOp) M.<|> (Other <$> asciiAlphaOrHyphen)
  where
    asciiAlphaOrHyphen =
      M.takeWhile1P
        (Just "asciiAlphaOrHyphen")
        (\c -> C.isAsciiUpper c || C.isAsciiLower c || c == '-')

builtIn :: Parser BuiltIn
builtIn = anInt M.<|> aBinOp M.<|> aStackOp
  where
    int = MCL.decimal
    anInt = AnInt <$> int
    aBinOp = ABinOp <$> binOp
    aStackOp = AStackOp <$> stackOp

stackOp :: Parser StackOp
stackOp = dup M.<|> drop' M.<|> swap M.<|> over
  where
    dup = Dup F.<$ MC.string' "DUP"
    drop' = Drop F.<$ MC.string' "DROP"
    swap = Swap F.<$ MC.string' "SWAP"
    over = Over F.<$ MC.string' "OVER"

binOp :: Parser BinOp
binOp = add M.<|> sub M.<|> mul M.<|> div'
  where
    add = Add F.<$ MC.char '+'
    sub = Sub F.<$ MC.char '-'
    mul = Mul F.<$ MC.char '*'
    div' = Div F.<$ MC.char '/'

spaceConsumer :: Parser ()
spaceConsumer = MCL.space MC.space1 M.empty M.empty

-- A parser that discards trailing space
lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme spaceConsumer
