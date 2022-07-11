module Bob (responseFor) where
import qualified Data.Char as C

countMatching :: (Char -> Bool) -> String -> Int
countMatching f xs = length [c | c <- xs, f c]

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = C.isAsciiUpper c || C.isAsciiLower c || C.isDigit c

responseFor :: String -> String
responseFor xs
  | addr = "Fine. Be that way!"
  | yell && ques = "Calm down, I know what I'm doing!"
  | yell = "Whoa, chill out!"
  | ques = "Sure."
  | otherwise = "Whatever."
  where
    s = [c | c <- xs, isAsciiAlphaNum c || c == '?']
    numUpper = countMatching C.isUpper s
    numLower = countMatching C.isLower s
    ques = last s == '?'
    addr = all C.isSpace s
    yell = numLower == 0 && numUpper > 0
