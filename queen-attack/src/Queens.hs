module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines board
  where
    board = map go xs
    xs = [0 .. 7]
    go row = unwords $ map (go' row) xs
    go' r c =
      let x = Just (r, c)
       in case () of
            _
              | x == white -> "W"
              | x == black -> "B"
              | otherwise -> "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (rank1, file1) (rank2, file2) =
  rank1 == rank2
    || file1 == file2
    || abs (rank1 - rank2) == abs (file1 - file2)
