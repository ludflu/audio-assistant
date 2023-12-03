{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Actions (findResponseRegex)
import ChatLogger
import qualified Codec.Binary.UTF8.Generic as B
import ConfigParser (EnvConfig (dbHost, dbName, dbPassword, dbUser, localpath, wavpath), parseConfig)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Concurrent.STM (STM, TQueue, atomically, newTQueueIO, readTVar)
import Control.Monad (unless)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.State (liftIO)
import qualified Data.ByteString as B
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.Persist.Postgresql (ConnectionPool, PostgresConf (PostgresConf, pgConnStr, pgPoolIdleTimeout, pgPoolSize, pgPoolStripes), createPostgresqlPoolWithConf, defaultPostgresConfHooks, withPostgresqlPool, withPostgresqlPoolWithConf)
import Listener
  ( ListenerMonad,
    ListenerState,
    audioReset,
    dbPool,
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

runJob :: EnvConfig -> ListenerState -> IO ()
runJob config startState = do
  print "starting audio-assistant"
  recorderThread <- forkIO $ record config (audioReset startState) 0
  threadDelay 4000000 -- wait 4 seconds for the recording thread to start
  runListenerMonad commandLoop config startState
  print "ending program, killing recorder thread"
  killThread recorderThread
  run config -- dirty hack because things sometimes crash

makeDbConfig :: EnvConfig -> Maybe PostgresConf
makeDbConfig config = do
  user <- dbUser config
  pwd <- dbPassword config
  host <- dbHost config
  dbname <- dbName config
  let connStr = concat ["host=", host, " port=5432 user=", user, " password=", pwd, " dbname=", dbname]
   in return $ PostgresConf {pgConnStr = B.fromString connStr, pgPoolSize = 2, pgPoolIdleTimeout = 10, pgPoolStripes = 1}

run :: EnvConfig -> IO ()
run config = do
  currentTime <- getCurrentTime
  currentWorkingDirectory <- getCurrentDirectory
  emptyMailbox :: TQueue String <- newTQueueIO
  shouldAudioReset :: MVar FilePath <- newEmptyMVar
  vad <- Vad.create
  let _config =
        if localpath config == ""
          then config {localpath = currentWorkingDirectory}
          else config
      outpath =
        if null $ wavpath config
          then currentWorkingDirectory
          else wavpath config
      startState' = initialState currentTime vad shouldAudioReset emptyMailbox (outpath ++ "/in0.wav")
      dbConfig = makeDbConfig _config
   in case dbConfig of
        Just pgConfig -> do
          runStdoutLoggingT $ withPostgresqlPoolWithConf pgConfig defaultPostgresConfHooks $ \pool -> do
            let startState = startState' (Just pool)
             in liftIO $ do
                  runMigrations (Just pool)
                  runJob _config startState
        Nothing -> do
          let startState = startState' Nothing
           in liftIO $ runJob _config startState

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
