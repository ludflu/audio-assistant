{-# LANGUAGE OverloadedStrings #-}

module Reminders where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad.State (liftIO)
import Listener (ListenerMonad, say)

runReminder :: Int -> String -> ListenerMonad ()
runReminder seconds reminder = do
  liftIO $ threadDelay (10 ^ 6 * seconds)
  say reminder
  return ()

setReminder :: Int -> String -> ListenerMonad ()
setReminder seconds reminder =
  let runAction = runReminder seconds reminder
   in do
        runAction
        return ()
