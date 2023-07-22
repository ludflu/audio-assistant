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
import MatchHelper (parseInt)

sendReminder :: Integer -> String -> MVar String -> IO ()
sendReminder seconds reminder mailbox = do
  threadDelay (10 ^ 6 * fromInteger seconds)
  putMVar mailbox reminder
  return ()

setReminder :: [String] -> ListenerMonad String
setReminder s = do
  liftIO $ print "setting reminder:---------"
  liftIO $ print s
  let seconds = parseInt $ head s
   in case seconds of
        Just secs -> setReminder' secs >> return "reminder set."
        Nothing -> return "Invalid time"

setReminder' :: Integer -> ListenerMonad ()
setReminder' seconds = do
  listener <- get
  say "what's the reminder?"
  reminder <- listenPatiently
  let box = mailbox listener
  liftIO $ sendReminder seconds reminder box
