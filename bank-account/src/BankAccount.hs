module BankAccount
  ( BankAccount,
    closeAccount,
    getBalance,
    incrementBalance,
    openAccount,
  )
where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as S

newtype BankAccount = BankAccount (TVar (Maybe Integer))

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount balance) =
  S.atomically $
    S.writeTVar balance Nothing

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount balance) =
  S.atomically $
    S.readTVar balance

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount balance) amount = S.atomically $
  -- need a do block if there're more than one statements
  do
    S.modifyTVar balance (fmap (+ amount))
    S.readTVar balance

openAccount :: IO BankAccount
openAccount = fmap BankAccount $ S.newTVarIO $ Just 0
