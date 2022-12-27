module Phone2 (number) where

import qualified Control.Applicative as A
import Control.Monad (void)
import qualified Data.Char as C
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

number :: String -> Maybe String
-- Force parser to consume entire input
-- <* Sequence actions, discarding the value of the second argument.
number xs = case M.parse (phoneNum A.<* M.eof) "" xs of
  -- :t err = M.ParseErrorBundle String Void
  Left err ->
    let _ = putStr $ M.errorBundlePretty err
     in Nothing
  Right x -> Just x

type Parser =
  M.Parsec
    -- The type for custom error messages. We have none, so use `Void`.
    Void
    -- The input stream type. Let's use `String` for now.
    String

phoneNum :: Parser String
phoneNum = M.skipManyTill filler $ do
  _ <- M.optional countryCode
  x <- threeDigits
  y <- threeDigits
  z <- fourDigits
  return $ x ++ y ++ z

-- Only 1 is considered a valid country code; if there's a '+',
-- it's considered as filler
countryCode :: Parser Char
countryCode = lexeme $ MC.char '1'

-- Parse a char between 'start' and 'end'
between :: Char -> Char -> Parser Char
between start end = M.satisfy (\x -> x >= start && x <= end)

-- Parse three digits
threeDigits :: Parser String
{-
'count' returns a list; concat the result from the first parser with the list.
Need to parenthesize the 2nd argument to 'count' because $ has very low precedence,
while <$> and <*> have higher precedence.
-}
threeDigits = lexeme $ (:) <$> between '2' '9' <*> M.count 2 (between '0' '9')

-- Parse 4 digits
fourDigits :: Parser String
fourDigits = lexeme $ M.count 4 $ between '0' '9'

-- Fillers fill the space between tokens
filler :: Parser ()
filler = void $ M.some $ M.satisfy (not . C.isDigit)

-- 3rd and 4th arguments are for ignoring comments
spaceConsumer :: Parser ()
spaceConsumer = L.space filler A.empty A.empty

-- A parser that discards trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
