module Fork 
    ( forkIOWait
    , joinThreads
    ) where


import Control.Concurrent
import Control.Exception.Base
import Control.Monad


forkIOWait :: IO () -> IO (MVar ())
forkIOWait io = 
    do m <- newEmptyMVar
       tid <- forkFinally io (\r -> handleTerminate m r)
       return m

joinThreads :: [MVar ()] -> IO ()
joinThreads = mapM_ takeMVar

handleTerminate :: MVar () -> Either SomeException () -> IO ()
handleTerminate _ (Left exception) = print exception
handleTerminate lock _ = putMVar lock ()
