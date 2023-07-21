{-# LANGUAGE OverloadedStrings #-}

module Reminders where

import System.Cron.Schedule (MonadSchedule (addJob), execSchedule)

runSchedule :: IO ()
runSchedule = do
  tids <- execSchedule $ do
    addJob job1 "* * * * *"
    addJob job2 "0 * * * *"
  print tids

job1 :: IO ()
job1 = putStrLn "Job 1"

job2 :: IO ()
job2 = putStrLn "Job 2"