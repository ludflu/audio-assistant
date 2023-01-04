{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where
    
import Control.Monad.State (liftIO)
import Sound.VAD.WebRTC as Vad ( create )

import Actions ( findResponseRegex )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import Listener
import System.Directory (getCurrentDirectory)

-- get a user query, we regex match the recognized voice text against possible known queries
-- and if it matches a known question, we generate an answer
-- and send it to the voice synthesis module

commandLoop :: ListenerMonad ()
commandLoop = do query <- listen
                 response <- findResponseRegex query
                 mapM_ say response
                 commandLoop

main :: IO ()
main = do currentTime <- getCurrentTime
          currentWorkingDirectory <- getCurrentDirectory
          print "vad-listener start"
          print currentTime
          vad <- Vad.create
          let config = EnvConfig currentWorkingDirectory
              startState = initialState currentTime vad
          runListenerMonad commandLoop config startState
          return ()
