module Robot where

import System.Random (randomR, getStdRandom)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)

data Robot = Robot {
    name :: String
}

robotName :: IORef Robot -> IO String
robotName ref = do
    robot <- readIORef ref
    return $ name robot

mkRobot :: IO (IORef Robot)
mkRobot = do
    newn <- newName
    newIORef $ Robot newn

resetName :: IORef Robot -> IO ()
resetName ref = do
    nname <- newName
    modifyIORef ref . const $ Robot nname

newName :: (IO String)
newName = do
    a <- letter
    b <- letter
    c <- number
    d <- number
    e <- number
    return [a, b, c, d, e]
    where letter = next 'A' 'Z'
          number = next '0' '9'
          next l g = getStdRandom (randomR (l, g))