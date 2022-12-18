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

type AudioWriter m a = (MonadResource m, Snd.Sample a) => (AudioSource m a) -> FilePath -> m()

audioRate :: Double = 16000
workingChunkSize :: Frames = round $ audioRate * 30 / 1000 

castInt :: Double -> Int16
castInt d = let maxint16 = 32768.0
                is = d * maxint16
	     in if (is > maxint16)
	        then (round maxint16)
		else if (is < -maxint16)
		     then round (- maxint16)
		     else round is

castInts :: [V.Vector Double] -> [V.Vector Int16]
castInts ds = map (V.map castInt) ds

printSamples :: [V.Vector Int16] -> [Int]
printSamples  ss = let sizes = \s -> V.length s
		    in map sizes ss


runSamples :: Vad.VAD RealWorld -> [V.Vector Int16] -> IO [Bool]
runSamples vd ss = let func = \s -> Vad.process (round audioRate) s vd
                       validFrames = filter (\vs -> V.length vs == workingChunkSize) ss
		    in mapM func validFrames

myformat :: Snd.Format
myformat =  Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

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

getWavFrom :: (MonadResource m, Snd.Sample a) => FilePath -> Double -> IO (AudioSource m a)
getWavFrom fp start = do src <- sourceSndFrom (Seconds start) fp
			 let split = DCA.splitChannels src -- we only want a single channel
			     reorged = DCA.reorganize workingChunkSize (head split)
			 return reorged

audioWriter :: AudioWriter (ResourceT IO) Double
audioWriter src fp = sinkSnd fp myformat src

getWavFromRec :: FilePath -> Double -> Double-> AudioWriter (ResourceT IO) Double -> Vad.VAD RealWorld -> IO ()
getWavFromRec fp start limit writer vad =
     if (start < limit) then
	do src  <- getWavFrom fp start
	   putStrLn "got source"
	   let twoSecs = takeStart (Seconds 2) src
               length = DCA.framesToSeconds (frames twoSecs) audioRate
	       samples = DCA.source src
	       channelCount = DCA.channels src
	       ending = start + length
	       elapsed = if (length >0) then ending else ending +1
               newpath = "./tmp/file" ++ show ending ++ ".wav"
	   putStrLn ("writing " ++ newpath)
	   putStrLn ("channel count: " ++ show channelCount)
	   ss <- runResourceT $ samples $$ sinkList
	   let rounded = castInts ss
	       sizes = printSamples rounded
	   bla <- runSamples vad rounded
--	   putStrLn ("is voice: " ++ show bla)
--	   putStrLn ("sample sizes: " ++ show sizes)
	   _ <- if (length >0)
		then 
                  runResourceT $ writer twoSecs newpath
	        else 
		  threadDelay 1000000
	   getWavFromRec fp elapsed limit writer vad
     else 
        putStrLn "Done!"

isValidRateAndFrame = Vad.validRateAndFrameLength (round audioRate) workingChunkSize
	
main :: IO ()
main = do vad <- Vad.create
          putStrLn ("Rate: " ++ show audioRate)
          putStrLn ("frameSize: " ++ show workingChunkSize)
	  putStrLn ("Valid audio parameters: " ++ show isValidRateAndFrame)
	  getWavFromRec "in.wav" 0.0 10.0 audioWriter vad



