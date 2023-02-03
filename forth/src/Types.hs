module Types where

import Data.Map (Map)
import Data.Text (Text)

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord Text
  deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int],
    defnDict :: Map Text [(Int, Text)],
    defnId :: Int
  }
  deriving (Show)

type ForthResult = Either ForthError ForthState
