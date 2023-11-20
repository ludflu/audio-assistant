{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecordNote where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.State (liftIO)
import Listener
  ( ListenerMonad,
    listenPatiently,
    say,
  )
import MatchHelper (isNo, isYes)
import System.IO (IOMode (ReadWriteMode), hGetContents', openFile)

record :: ListenerMonad String
record = do
  say "What should the note say?"
  listenPatiently

say_ :: String -> ListenerMonad ()
say_ note = do
  _ <- say note
  return ()

askQuestion :: String -> ListenerMonad Bool
askQuestion q = do
  say q
  a <- listenPatiently
  if isYes a
    then return True
    else
      if isNo a
        then return False
        else say "Please say 'yes, affirmative' or 'no, negative'" >> askQuestion q

recordNote :: ListenerMonad String
recordNote = do
  note <- record
  let fname = "test.txt"
  _ <- liftIO $ writeFile fname note
  shouldRead <- askQuestion "Done. Should I read it back?"
  when shouldRead (say_ note)
  return ""

readNote :: ListenerMonad String
readNote = do
  let fname = "test.txt"
  handle <- liftIO $ openFile fname ReadWriteMode
  liftIO $ hGetContents' handle