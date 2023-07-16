{-# LANGUAGE ScopedTypeVariables #-}

module VoiceDetectionSliceReader where

import Conduit
  ( MonadIO (liftIO),
    MonadResource,
    PrimMonad (PrimState),
    runConduit,
    runConduitRes,
    runResourceT,
    sinkList,
    ($$),
    (.|),
  )
import Control.Monad.ST (RealWorld)
import Control.Retry
import Data.Conduit.Audio as DCA
  ( AudioSource (frames, source),
    Duration (Seconds),
    Frames,
    framesToSeconds,
    integralSample,
    mapSamples,
    reorganize,
    splitChannels,
    takeStart,
  )
import Data.Conduit.Audio.SampleRate ()
import Data.Conduit.Audio.Sndfile
  ( sinkSnd,
    sourceSnd,
    sourceSndFrom,
  )
import Data.Int (Int16)
import qualified Data.Vector.Storable as V
import qualified Sound.File.Sndfile as Snd
import Sound.VAD.WebRTC as Vad
  ( VAD,
    create,
    process,
    validRateAndFrameLength,
  )

myformat :: Snd.Format
myformat = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

timeChunk :: Double
timeChunk :: Double = 30.0 / 1000.0

workingChunkSize :: Double -> Frames
workingChunkSize rate = round $ rate * timeChunk

convertIntegral :: (MonadResource m) => AudioSource m Double -> AudioSource m Int16
convertIntegral = DCA.mapSamples DCA.integralSample

detectVoice :: PrimMonad m => Vad.VAD (PrimState m) -> [V.Vector Int16] -> Double -> m [Bool]
detectVoice vd ss rate =
  let func s = Vad.process (round rate) s vd
      validFrames = filter (\vs -> V.length vs == workingChunkSize rate) ss
   in mapM func validFrames

-- given a path, a start time and a duration (in seconds) will return:
-- a single channel audio source,
-- with pulses recorded as Int16s
-- in chunks of 30 ms
getWavFrom :: (MonadResource m) => FilePath -> Double -> Double -> Double -> IO (AudioSource m Int16)
getWavFrom fp start dur rate = do
  src <- sourceSndFrom (Seconds start) fp
  let split = DCA.splitChannels src -- we only want a single channel
      reorged = DCA.reorganize (workingChunkSize rate) (head split)
      timelimited = takeStart (Seconds dur) reorged
  return $ convertIntegral timelimited

writeBoundedWave :: FilePath -> FilePath -> Double -> Double -> Double -> IO ()
writeBoundedWave oldPath newPath start end rate =
  do
    src <- getWavFrom oldPath start (end - start) rate
    runResourceT $ sinkSnd newPath myformat src

readSlice :: FilePath -> Double -> Double -> Double -> Vad.VAD RealWorld -> IO ([Bool], Double)
readSlice path start end rate vad = do
  src <- getWavFrom path start end rate
  let length = DCA.framesToSeconds (frames src) rate
      samples = DCA.source src
  ss <- liftIO $ runConduitRes $ samples .| sinkList
  activations <- liftIO $ detectVoice vad ss rate
  return (activations, length)

exponentialPolicy :: RetryPolicy
exponentialPolicy = exponentialBackoff 200000 <> limitRetries 3

-- The function to decide whether to retry or not.
-- This takes the current status of the retries (how
-- often has been retried already and for how long) and
-- returns whether we should retry or not.
-- retryDecider :: RetryStatus -> b -> m Bool
-- retryDecider _ x = return x

readSliceWithRetry :: FilePath -> Double -> Double -> Double -> Vad.VAD RealWorld -> IO ([Bool], Double)
readSliceWithRetry path start end rate vad =
  let f _ = readSlice path start end rate vad
   in recoverAll exponentialPolicy f
