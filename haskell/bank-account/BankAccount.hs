module BankAccount (BankAccount, openAccount, closeAccount, getBalance, incrementBalance ) where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)

data BankAccount = BankAccount {
    value :: IORef Integer,
    open :: IORef Bool
}

openAccount :: IO BankAccount
openAccount = do
    valRef <- newIORef 0
    openRef <- newIORef True
    return $ BankAccount valRef openRef

closeAccount :: BankAccount -> IO ()
closeAccount acct = writeIORef (open acct) False

getBalance :: BankAccount -> IO(Maybe Integer)
getBalance acct = do
    isOpen <- readIORef (open acct)
    balance <- readIORef (value acct)
    return $ possible isOpen balance
    where possible op b = if op then Just b else Nothing

incrementBalance :: BankAccount -> Integer -> IO(Maybe Integer)
incrementBalance acct add = do
    modifyIORef (value acct) (+add)
    getBalance acct