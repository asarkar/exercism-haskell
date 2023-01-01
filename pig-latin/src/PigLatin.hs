module PigLatin (translate) where

import Data.Text (Text)
import qualified Data.Text as T

translate :: Text -> Text
translate xs = (T.unwords . map go) $ T.words xs
  where
    go s = find rules s <> T.pack "ay"
    rules =
      [ startsWithVowel,
        startsWithVowelSound,
        startsWithConsonantFollowedByQu,
        containsVowel,
        startsWithConsonantFollowedByY
      ]
    find [] s = s
    find (rule : rest) s = case rule s of
      Just x -> x
      _ -> find rest s

startsWithVowel :: Text -> Maybe Text
startsWithVowel xs
  | match = Just xs
  | otherwise = Nothing
  where
    (left, _) = T.break (`elem` "aeiou") xs
    match = T.null left

startsWithVowelSound :: Text -> Maybe Text
startsWithVowelSound xs
  | match = Just xs
  | otherwise = Nothing
  where
    match = T.isPrefixOf (T.pack "xr") xs || T.isPrefixOf (T.pack "yt") xs

startsWithConsonantFollowedByQu :: Text -> Maybe Text
startsWithConsonantFollowedByQu xs
  | match = Just (y <> left <> x)
  | otherwise = Nothing
  where
    (left, right) = T.breakOn (T.pack "qu") xs
    match = (not . T.null) right
    (x, y) = T.splitAt 2 right

containsVowel :: Text -> Maybe Text
containsVowel xs
  | match = Just (right <> left)
  | otherwise = Nothing
  where
    (left, right) = T.break (`elem` "aeiou") xs
    match = (not . T.null) left && (not . T.null) right

startsWithConsonantFollowedByY :: Text -> Maybe Text
startsWithConsonantFollowedByY xs
  | match = Just (right <> left)
  | otherwise = Nothing
  where
    (left, right) = T.break (== 'y') xs
    match =
      (T.length left >= 2 && (not . T.null) right)
        || (T.length left == 1 && T.length right == 1)
