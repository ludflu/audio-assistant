{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Conduit
import Data.Conduit.Audio as DCA
import Data.Conduit.Audio.Sndfile
import Data.Conduit.Audio.SampleRate
import qualified Sound.File.Sndfile as Snd
import Control.Concurrent
import Sound.VAD.WebRTC as Vad
import qualified Data.Vector.Storable as V
import qualified Data.Conduit.List as CL
import Control.Monad.ST
import Data.Int (Int16)
import Control.Monad

audioRate :: Double = 16000
workingChunkSize :: Frames = round $ audioRate * 30 / 1000 

printSamples :: [V.Vector Int16] -> [Int]
printSamples  ss = let sizes = \s -> V.length s
		    in map sizes ss


runSamples :: Vad.VAD RealWorld -> [V.Vector Int16] -> IO [Bool]
runSamples vd ss = let func = \s -> Vad.process (round audioRate) s vd
                       validFrames = filter (\vs -> V.length vs == workingChunkSize) ss
		    in mapM func validFrames

myformat :: Snd.Format
myformat =  Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile


convertIntegral :: (MonadResource m) => AudioSource m Double -> AudioSource m Int16
convertIntegral src = DCA.mapSamples DCA.integralSample src

getwavs :: (MonadResource m, Snd.Sample a) => [FilePath] -> IO ([AudioSource m a])
getwavs fps = mapM getwav fps

getwav :: (MonadResource m, Snd.Sample a) => FilePath -> IO (AudioSource m a)
getwav fp = sourceSnd fp

concatWavs:: (MonadResource m, Snd.Sample a) => [FilePath] -> IO (AudioSource m a)
concatWavs fps = do as <- getwavs fps
		    let first = head as
			rest = tail as 
		        combined = foldl DCA.concatenate first rest
		    return combined

-- given a path, a start time and a duration (in seconds) will return:
-- a single channel audio source, 
-- with pulses recorded as Int16s
-- in chunks of 30 ms
getWavFrom :: (MonadResource m) => FilePath -> Double -> Double -> IO (AudioSource m Int16)
getWavFrom fp start dur = do src <- sourceSndFrom (Seconds start) fp
			     let split = DCA.splitChannels src -- we only want a single channel
			         reorged = DCA.reorganize workingChunkSize (head split)
				 timelimited = takeStart (Seconds dur) reorged
				 is = convertIntegral timelimited
			     return is


getWavFromRec :: FilePath -> Double -> Double -> Vad.VAD RealWorld -> IO ()
getWavFromRec fp start limit vad =
     if (start < limit) then
	do src <- getWavFrom fp start 2.0
	   putStrLn "got source"
	   let length = DCA.framesToSeconds (frames src) audioRate
	       samples = DCA.source src
	       ending = start + length
	       elapsed = if (length >0) then ending else ending +1
               newpath = "./tmp/file" ++ show ending ++ ".wav"
	   putStrLn ("writing " ++ newpath)
	   ss <- runResourceT $ samples $$ sinkList --get samples from conduit
	   voiceDetected <- runSamples vad ss
	   putStrLn ("is voice: " ++ show voiceDetected)
	   _ <- if (length >0)
		then 
		  runResourceT $ sinkSnd newpath myformat src
	        else 
		  threadDelay 1000000
	   getWavFromRec fp elapsed limit vad
     else 
        putStrLn "Done!"

isValidRateAndFrame = Vad.validRateAndFrameLength (round audioRate) workingChunkSize
	
main :: IO ()
main = do vad <- Vad.create
	  getWavFromRec "in.wav" 0.0 10.0 vad



