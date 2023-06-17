{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RecordNote where

import System.Random
import Listener
import Control.Monad.State (liftIO)
import Data.Maybe
import Control.Concurrent (threadDelay)
import System.IO

record :: ListenerMonad String
record = do say "What should the note say?"
            listenPatiently
                         
recordNote :: ListenerMonad String
recordNote = do note <- record
                let fname = "test.txt"
                _ <- liftIO $ writeFile fname note
                shouldRead <- askQuestion "Done. Should I read it back?"
                if shouldRead
                   then say note
                   else return "ok."
                         
readNote :: ListenerMonad String
readNote = do let fname = "test.txt"
              handle <- liftIO $ openFile fname ReadWriteMode
              contents <- liftIO $ hGetContents' handle
              return contents

