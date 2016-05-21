module Deque where

import           Data.IORef    (IORef, newIORef, readIORef, writeIORef)
import           Data.Maybe    (fromMaybe, isNothing)

import           Control.Monad (when)

data Item a = Item {
    next  :: IORef (Maybe (Item a)),
    prev  :: IORef (Maybe (Item a)),
    value :: a
} deriving (Eq)

data Deque a = Deque {
    start :: IORef (Maybe (Item a)),
    end   :: IORef (Maybe (Item a))
}


push :: Deque a -> a -> IO ()
push = add start end next prev

unshift :: Deque a -> a -> IO ()
unshift = add end start prev next

pop :: Deque a -> IO (Maybe a)
pop = remove start end next

shift :: Deque a -> IO (Maybe a)
shift = remove end start prev


mkItem :: a -> IO (Item a)
mkItem a = do
    n <- newIORef Nothing :: IO (IORef (Maybe (Item a)))
    p <- newIORef Nothing
    return Item {next = n, prev = p, value = a}

mkDeque :: IO (Deque a)
mkDeque = do
    s <- newIORef Nothing
    e <- newIORef Nothing
    return $ Deque s e


add :: (Deque a -> IORef (Maybe (Item a)))
    -> (Deque a -> IORef (Maybe (Item a)))
    -> (Item a -> IORef (Maybe (Item a)))
    -> (Item a -> IORef (Maybe (Item a)))
    -> Deque a
    -> a
    -> IO ()
add dequeFn dequeFn' itemFn itemFn' deque item = do
    newItem <- mkItem item
    lastItemM <- readIORef (dequeFn deque)
    writeIORef (itemFn newItem) lastItemM
    writeIORef (dequeFn deque) (Just newItem)
    case lastItemM of
        Nothing       -> writeIORef (dequeFn' deque) (Just newItem)
        Just lastItem -> writeIORef (itemFn' lastItem) (Just newItem)

remove :: (Deque a -> IORef (Maybe (Item a)))
       -> (Deque a -> IORef (Maybe (Item a)))
       -> (Item a -> IORef (Maybe (Item a)))
       -> Deque a
       -> IO (Maybe a)
remove dequeFn dequeFn' itemFn deque = do
    lastItemM <- readIORef (dequeFn deque)
    prevItemM <- fromMaybe (return Nothing) (readIORef . itemFn <$> lastItemM)
    writeIORef (dequeFn deque) prevItemM
    when (isNothing lastItemM) $ writeIORef (dequeFn' deque) Nothing
    return $ value <$> lastItemM
