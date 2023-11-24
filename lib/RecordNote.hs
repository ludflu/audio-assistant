{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecordNote where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.State (liftIO)
import Listener
  ( ListenerMonad,
    listenPatiently,
    writeToMailBox,
  )
import MatchHelper (isNo, isYes)
import System.IO (IOMode (ReadWriteMode), hGetContents', openFile)

record :: ListenerMonad String
record = do
  writeToMailBox "What should the note say?"
  listenPatiently

say_ :: String -> ListenerMonad ()
say_ note = do
  _ <- writeToMailBox note
  return ()

askQuestion :: String -> ListenerMonad Bool
askQuestion q = do
  writeToMailBox q
  a <- listenPatiently
  if isYes a
    then return True
    else
      if isNo a
        then return False
        else writeToMailBox "Please say 'yes, affirmative' or 'no, negative'" >> askQuestion q

recordNote :: ListenerMonad ()
recordNote = do
  note <- record
  let fname = "test.txt"
  _ <- liftIO $ writeFile fname note
  shouldRead <- askQuestion "Done. Should I read it back?"
  when shouldRead (writeToMailBox note)

getNote :: IO String
getNote = do
  let fname = "test.txt"
  handle <- openFile fname ReadWriteMode
  hGetContents' handle

readNote :: ListenerMonad ()
readNote = do
  msg <- liftIO getNote
  writeToMailBox msg
