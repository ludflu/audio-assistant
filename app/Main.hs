{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Conduit
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Data.Conduit.Audio.SampleRate
import qualified Sound.File.Sndfile               as Snd
import Data.Conduit.Audio.LAME 
import Control.Concurrent

data VadChunk = VadChunk [Double] Bool


type AudioWriter m a = (MonadResource m, Snd.Sample a) => (AudioSource m a) -> FilePath -> m()
audioRate :: Double = 44100.0

	{-
vadscan :: FilePath -> Double -> Int -> [VadChunk] -> IO ()
vadscan audioFile start chunkSize labledChunks = do
  src <- sourceSndFrom (Seconds start) audioFile
  putStrLn "hello!"
-}

myformat :: Snd.Format
myformat =  Snd.Format Snd.HeaderFormatWav  Snd.SampleFormatPcm16 Snd.EndianFile

getwav :: (MonadResource m, Snd.Sample a) => FilePath -> IO (AudioSource m a)
getwav fp = sourceSnd fp

getWavFrom :: (MonadResource m, Snd.Sample a) => FilePath -> Double -> IO (AudioSource m a)
getWavFrom fp start = sourceSndFrom (Seconds start) fp

audioWriter :: AudioWriter (ResourceT IO) Double
audioWriter src fp = sinkSnd fp myformat src

getWavFromRec :: FilePath -> Double -> Double-> AudioWriter (ResourceT IO) Double -> IO ()
getWavFromRec fp start limit writer =
     if (start < limit) then
	do src  <- getWavFrom fp start
	   putStrLn "got source"
	   let twoSecs = takeStart (Seconds 2) src
               length = framesToSeconds (frames twoSecs) audioRate
	       ending = start + length
	       elapsed = if (length >0) then ending else ending +1
               newpath = "./tmp/file" ++ show ending ++ ".wav"
	   putStrLn ("writing " ++ newpath)
	   _ <- if (length >0)
		then 
                  runResourceT $ writer twoSecs newpath
	        else 
		  threadDelay 1000000
	   getWavFromRec fp elapsed limit writer
     else 
        putStrLn "Done!"

	
main :: IO ()
main = getWavFromRec "in.wav" 0.0 20.0 audioWriter
