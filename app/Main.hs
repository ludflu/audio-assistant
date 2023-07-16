{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Actions (findResponseRegex)
import ConfigParser (EnvConfig (localpath, wavpath), parseConfig)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad (unless)
import Control.Monad.State (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Listener
  ( ListenerMonad,
    initialState,
    listen,
    runListenerMonad,
    say,
    shouldQuit,
  )
import Options.Applicative
  ( execParser,
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
  mapM_ say response
  quit <- shouldQuit
  unless quit commandLoop

run :: EnvConfig -> IO ()
run config = do
  currentTime <- getCurrentTime
  currentWorkingDirectory <- getCurrentDirectory
  shouldReset :: MVar FilePath <- newEmptyMVar
  print "vad-listener start"
  print currentTime
  print $ show config
  vad <- Vad.create
  let _config =
        if localpath config == ""
          then config {localpath = currentWorkingDirectory}
          else config
      outpath =
        if null $ wavpath config
          then currentWorkingDirectory
          else wavpath config
      startState = initialState currentTime vad shouldReset (outpath ++ "/in0.wav")
  recorderThread <- forkIO $ record _config shouldReset 0
  threadDelay 4000000 -- wait one second for the recording thread to start
  runListenerMonad commandLoop _config startState
  print "ending program, killing recorder thread"
  killThread recorderThread
  return ()

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
