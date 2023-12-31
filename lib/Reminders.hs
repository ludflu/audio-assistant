{-# LANGUAGE OverloadedStrings #-}

module Reminders where

import Control.Concurrent (MVar, ThreadId, forkIO, putMVar, threadDelay)
import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTQueue)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    gets,
    lift,
    liftIO,
  )
import Listener (ListenerMonad, ListenerState (mailbox), listenPatiently, say, writeToMailBox)
import MatchHelper (readInt)

sendReminder' :: Integer -> String -> TQueue String -> IO ()
sendReminder' seconds reminder mailbox = do
  threadDelay (10 ^ 6 * fromInteger seconds * 60)
  liftIO $ atomically $ writeTQueue mailbox reminder
  return ()

sendReminder :: Integer -> String -> TQueue String -> IO ()
sendReminder seconds reminder mailbox = do
  forkIO $ sendReminder' seconds reminder mailbox
  return ()

setReminder' :: Integer -> ListenerMonad ()
setReminder' seconds = do
  listener <- get
  writeToMailBox "what's the reminder?"
  reminder <- listenPatiently
  let box = mailbox listener
      reminder' = "Reminder: " ++ reminder
  liftIO $ sendReminder seconds reminder' box

setReminder :: [String] -> ListenerMonad ()
setReminder s = do
  liftIO $ print "setting reminder:---------"
  liftIO $ print s
  let seconds = readInt $ last s
   in case seconds of
        Just secs -> setReminder' secs >> writeToMailBox "reminder set."
        Nothing -> writeToMailBox "Invalid time"
