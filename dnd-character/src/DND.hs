-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html
-- https://kodimensional.dev/recordwildcards
{-# LANGUAGE RecordWildCards #-}

module DND
  ( Character (..),
    ability,
    modifier,
    character,
  )
where

-- https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html
import qualified Test.QuickCheck as QC

data Character = Character
  { strength :: Int,
    dexterity :: Int,
    constitution :: Int,
    intelligence :: Int,
    wisdom :: Int,
    charisma :: Int,
    hitpoints :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier c = (c - 10) `div` 2

ability :: QC.Gen Int
ability = do
  ds <- QC.vectorOf 4 (QC.chooseInt (1, 6))
  return $ sum ds - minimum ds

character :: QC.Gen Character
character = do
  strength <- ability
  dexterity <- ability
  constitution <- ability
  intelligence <- ability
  wisdom <- ability
  charisma <- ability
  let hitpoints = 10 + modifier constitution
  return Character {..}
