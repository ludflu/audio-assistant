{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RecordAudio where

import ConfigParser (EnvConfig, localpath, recordingLength, wavpath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar)
import Control.Monad (when)
import Network.HaskellNet.IMAP.Connection (exists)
import System.Posix.Signals
import System.Process (createProcess, proc, waitForProcess)
import System.Process.Internals

makeFname :: FilePath -> Int -> FilePath
makeFname basepath count = basepath ++ "in" ++ show count ++ ".wav"

record :: EnvConfig -> MVar FilePath -> Int -> IO ()
record config reset count =
  let outpath =
        if null $ wavpath config
          then localpath config ++ "/"
          else wavpath config ++ "/"
      audioLength = recordingLength config
      command = localpath config ++ "/scripts/record.sh"
      fname = makeFname outpath count
   in do
        (_, _, _, phandle) <- createProcess (proc command [fname, show audioLength])
        print "waiting for process"
        waitForProcess phandle
        print $ "starting over! " ++ fname
        putMVar reset $ makeFname outpath (count + 1)
        record config reset (count + 1)
