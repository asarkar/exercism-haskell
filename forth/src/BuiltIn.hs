{-# LANGUAGE RecordWildCards #-}

module BuiltIn (runBuiltIn) where

import Grammar
  ( BinOp (..),
    BuiltIn (..),
    StackOp (..),
  )
import Types

runBuiltIn :: BuiltIn -> ForthState -> ForthResult
runBuiltIn b st@ForthState {..} = case b of
  AnInt i -> Right $ st {stack = i : stack}
  ABinOp op -> runBinOp op st
  AStackOp op -> runStackOp op st

runBinOp :: BinOp -> ForthState -> ForthResult
runBinOp op st@ForthState {..} = case stack of
  (x : y : xs) -> case op of
    Add -> Right $ st {stack = (x + y) : xs}
    Sub -> Right $ st {stack = (y - x) : xs}
    Mul -> Right $ st {stack = (x * y) : xs}
    Div | x /= 0 -> Right $ st {stack = (y `div` x) : xs}
    Div -> Left DivisionByZero
  _ -> Left StackUnderflow

runStackOp :: StackOp -> ForthState -> ForthResult
runStackOp op st@ForthState {..} = case (stack, op) of
  -- copy second stack item to top of stack
  (_ : y : _, Over) -> Right $ st {stack = y : stack}
  -- duplicate the top stack item
  (x : _, Dup) -> Right $ st {stack = x : stack}
  -- exchange the top 2 stack items
  (x : y : xs, Swap) -> Right $ st {stack = y : x : xs}
  -- remove top item from the stack
  (_ : xs, Drop) -> Right $ st {stack = xs}
  _ -> Left StackUnderflow
