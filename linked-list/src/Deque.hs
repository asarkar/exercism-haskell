module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Nil | Node (IORef (Node a)) a (IORef (Node a))

data Deque a = Deque (IORef (Node a)) (IORef (Node a))

mkDeque :: IO (Deque a)
mkDeque = do
  firstNodeRef <- newIORef Nil
  lastNodeRef <- newIORef Nil
  return (Deque firstNodeRef lastNodeRef)

pop :: Deque a -> IO (Maybe a)
pop (Deque firstNodeRef lastNodeRef) = do
  firstNode <- readIORef firstNodeRef
  case firstNode of
    Nil -> do
      return Nothing
    Node prevNodeRef x nextNodeRef -> do
      oldPrevNode <- readIORef prevNodeRef
      oldNextNode <- readIORef nextNodeRef
      case oldNextNode of
        Nil -> do
          writeIORef firstNodeRef oldPrevNode
          writeIORef lastNodeRef oldNextNode
          return (Just x)
        Node prevNodeRef' _ _ -> do
          writeIORef firstNodeRef oldNextNode
          writeIORef prevNodeRef' oldPrevNode
          return (Just x)

push :: Deque a -> a -> IO ()
push (Deque firstNodeRef lastNodeRef) x = do
  firstNode <- readIORef firstNodeRef
  case firstNode of
    Nil -> do
      newPrevNodeRef <- newIORef Nil
      newNextNodeRef <- newIORef Nil
      let newNode = Node newPrevNodeRef x newNextNodeRef
      writeIORef firstNodeRef newNode
      writeIORef lastNodeRef newNode
    Node prevNodeRef _ _ -> do
      oldPrevNode <- readIORef prevNodeRef
      newPrevNodeRef <- newIORef oldPrevNode
      oldNextNode <- readIORef firstNodeRef
      newNextNodeRef <- newIORef oldNextNode
      let newNode = Node newPrevNodeRef x newNextNodeRef
      writeIORef prevNodeRef newNode
      writeIORef firstNodeRef newNode

unshift :: Deque a -> a -> IO ()
unshift (Deque firstNodeRef lastNodeRef) x = do
  lastNode <- readIORef lastNodeRef
  case lastNode of
    Nil -> do
      newPrevNodeRef <- newIORef Nil
      newNextNodeRef <- newIORef Nil
      let newNode = Node newPrevNodeRef x newNextNodeRef
      writeIORef firstNodeRef newNode
      writeIORef lastNodeRef newNode
    Node _ _ nextNodeRef -> do
      oldPrevNode <- readIORef lastNodeRef
      newPrevNodeRef <- newIORef oldPrevNode
      oldNextNode <- readIORef nextNodeRef
      newNextNodeRef <- newIORef oldNextNode
      let newNode = Node newPrevNodeRef x newNextNodeRef
      writeIORef nextNodeRef newNode
      writeIORef lastNodeRef newNode

shift :: Deque a -> IO (Maybe a)
shift (Deque firstNodeRef lastNodeRef) = do
  lastNode <- readIORef lastNodeRef
  case lastNode of
    Nil -> do
      return Nothing
    Node prevNodeRef x nextNodeRef -> do
      oldPrevNode <- readIORef prevNodeRef
      oldNextNode <- readIORef nextNodeRef
      case oldPrevNode of
        Nil -> do
          writeIORef firstNodeRef oldPrevNode
          writeIORef lastNodeRef oldNextNode
          return (Just x)
        Node _ _ nextNodeRef' -> do
          writeIORef lastNodeRef oldPrevNode
          writeIORef nextNodeRef' oldNextNode
          return (Just x)
