{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecordAudio where

import ConfigParser (EnvConfig, localpath, recordingLength)
import Control.Concurrent (threadDelay)
import System.Posix.Signals
import System.Process
import System.Process.Internals
import Control.Concurrent.MVar


makeFname :: Int -> FilePath
makeFname count = "in" ++ show count ++ ".wav"

record :: EnvConfig -> MVar FilePath -> Int -> IO ()
record config reset count =
  let path = localpath config
      audioLength = recordingLength config
      command = path ++ "/scripts/record.sh"
      fname = makeFname count
   in do
        (_,_,_,phandle) <- createProcess (proc command [fname, show audioLength])
        print "waiting for process"
        waitForProcess phandle
        print $ "starting over! "  ++ fname
        putMVar reset $ makeFname (count+1)
        record config reset (count+1)
