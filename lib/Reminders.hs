{-# LANGUAGE OverloadedStrings #-}

module Reminders where

import Control.Concurrent (MVar, ThreadId, forkIO, putMVar, threadDelay)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    gets,
    lift,
    liftIO,
  )
import Listener (ListenerMonad, ListenerState (mailbox), listenPatiently, say)
import MatchHelper (readInt)

sendReminder' :: Integer -> String -> MVar String -> IO ()
sendReminder' seconds reminder mailbox = do
  threadDelay (10 ^ 6 * fromInteger seconds * 60)
  putMVar mailbox reminder
  return ()

sendReminder :: Integer -> String -> MVar String -> IO ()
sendReminder seconds reminder mailbox = do
  forkIO $ sendReminder' seconds reminder mailbox
  return ()

setReminder' :: Integer -> ListenerMonad ()
setReminder' seconds = do
  listener <- get
  say "what's the reminder?"
  reminder <- listenPatiently
  let box = mailbox listener
  liftIO $ sendReminder seconds reminder box

setReminder :: [String] -> ListenerMonad String
setReminder s = do
  liftIO $ print "setting reminder:---------"
  liftIO $ print s
  let seconds = readInt $ last s
   in case seconds of
        Just secs -> setReminder' secs >> return "reminder set."
        Nothing -> return "Invalid time"
