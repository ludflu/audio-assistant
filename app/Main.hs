{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Actions (findResponseRegex)
import ConfigParser (EnvConfig (localpath, wavpath), parseConfig)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM (STM, TQueue, atomically, newTQueueIO, readTVar)
import Control.Monad (unless)
import Control.Monad.State (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (ConnectionPool, PostgresConf (PostgresConf, pgConnStr, pgPoolIdleTimeout, pgPoolSize, pgPoolStripes), createPostgresqlPoolWithConf)
import Listener
  ( ListenerMonad,
    initialState,
    listen,
    runListenerMonad,
    say,
    shouldQuit,
  )
import Options.Applicative
  ( Alternative (empty),
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
  )
import RecordAudio (record)
import Sound.VAD.WebRTC as Vad (create)
import System.Directory (getCurrentDirectory)

-- get a user query, we regex match the recognized voice text against possible known queries
-- and if it matches a known question, we generate an answer
-- and send it to the voice synthesis module

commandLoop :: ListenerMonad ()
commandLoop = do
  query <- listen
  liftIO $ print query
  response <- findResponseRegex query
  liftIO $ print response
  quit <- shouldQuit
  unless quit commandLoop

run :: EnvConfig -> IO ()
run config = do
  currentTime <- getCurrentTime
  currentWorkingDirectory <- getCurrentDirectory
  shouldReset :: MVar FilePath <- newEmptyMVar

  emptyMailbox :: TQueue String <- newTQueueIO

  print "vad-listener start"
  print currentTime
  print $ show config
  vad <- Vad.create
  let pgconfig = PostgresConf {pgConnStr = "host=localhost port=5432 user=postgres password=<PASSWORD> dbname=vad", pgPoolSize = 2, pgPoolIdleTimeout = 10, pgPoolStripes = 1}
  pool <- createPostgresqlPoolWithConf pgconfig 2
  let _config =
        if localpath config == ""
          then config {localpath = currentWorkingDirectory}
          else config
      outpath =
        if null $ wavpath config
          then currentWorkingDirectory
          else wavpath config
      startState = initialState currentTime vad shouldReset emptyMailbox (outpath ++ "/in0.wav") (Just pool)
  recorderThread <- forkIO $ record _config shouldReset 0
  threadDelay 4000000 -- wait 4 seconds for the recording thread to start
  runListenerMonad commandLoop _config startState
  print "ending program, killing recorder thread"
  killThread recorderThread
  run config -- dirty hack because things sometimes crash

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseConfig <**> helper)
        ( fullDesc
            <> progDesc "run the voice assistant"
            <> header "vad-assist - a voice assistant"
        )
