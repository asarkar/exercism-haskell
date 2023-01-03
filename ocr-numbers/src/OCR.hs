module OCR (convert) where

import qualified Data.List as L

convert :: String -> String
convert = L.intercalate "," . map parseN . chunk4 . lines
  where
    -- Process 4 rows at a time
    chunk4 [] = []
    chunk4 xs = takeWhile (not . null) $ L.unfoldr (Just . splitAt 4) xs

parseN :: [String] -> String
parseN [] = ""
parseN xs = x ++ (if done then "" else parseN rest)
  where
    done = all null rest
    x = maybe "?" (show . fst) n
    n = L.find ((== firstNum) . snd) $ zip [0 :: Int ..] nums
    -- Process 3 columns at a time
    (firstNum, rest) = unzip $ map (splitAt 3) xs
    nums =
      [ [ " _ ",
          "| |",
          "|_|",
          "   "
        ],
        [ "   ",
          "  |",
          "  |",
          "   "
        ],
        [ " _ ",
          " _|",
          "|_ ",
          "   "
        ],
        [ " _ ",
          " _|",
          " _|",
          "   "
        ],
        [ "   ",
          "|_|",
          "  |",
          "   "
        ],
        [ " _ ",
          "|_ ",
          " _|",
          "   "
        ],
        [ " _ ",
          "|_ ",
          "|_|",
          "   "
        ],
        [ " _ ",
          "  |",
          "  |",
          "   "
        ],
        [ " _ ",
          "|_|",
          "|_|",
          "   "
        ],
        [ " _ ",
          "|_|",
          " _|",
          "   "
        ]
      ]
