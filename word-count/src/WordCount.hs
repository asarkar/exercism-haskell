module WordCount (wordCount) where
  
import qualified Data.Char as C
import qualified Data.MultiSet as MS
import qualified Data.Either as E
import qualified Control.Applicative as A
import Control.Monad (void)
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

wordCount :: String -> [(String, Int)]
wordCount xs = MS.toOccurList zs
  where
    ys = E.fromRight [] (WordCount.words xs)
    zs = MS.fromList $ [map C.toLower w | w <- ys]

type Parser =
  M.Parsec
    -- The type for custom error messages. We have none, so use `Void`.
    Void
    -- The input stream type. Let's use `String` for now.
    String

data Word = Number String | SimpleWord String | Contraction String deriving (Eq)

instance Show WordCount.Word where
  show (Number x) = x
  show (SimpleWord x) = x
  show (Contraction x) = x

words :: String -> Either String [String]
-- Force parser to consume entire input
-- <* Sequence actions, discarding the value of the second argument.
words input = case M.parse (M.some WordCount.word A.<* M.eof) "" input of
  -- :t err = M.ParseErrorBundle String Void
  Left err ->
    let e = M.errorBundlePretty err
        _ = putStr e
     in Left e
  Right x -> Right $ map show x

word :: Parser WordCount.Word
word =
  M.skipManyTill filler $
    lexeme $
      M.choice
        -- <$> is infix for 'fmap'
        [ Number <$> number,
          Contraction <$> M.try contraction,
          SimpleWord <$> simpleWord
        ]

number :: Parser String
number = M.some MC.numberChar

simpleWord :: Parser String
simpleWord = M.some MC.letterChar

contraction :: Parser String
contraction = do
  left <- simpleWord
  void $ MC.char '\''
  right <- simpleWord
  return $ left ++ "'" ++ right

-- Define separator characters
isSep :: Char -> Bool
isSep x = C.isSpace x || (not . C.isAlphaNum) x

-- Fillers fill the space between tokens
filler :: Parser ()
filler = void $ M.some $ M.satisfy isSep

-- 3rd and 4th arguments are for ignoring comments
spaceConsumer :: Parser ()
spaceConsumer = L.space filler A.empty A.empty

-- A parser that discards trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer