{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Numeric ( showFFloat )
import Conduit
    ( MonadIO(liftIO),
      PrimMonad(PrimState),
      sinkList,
      ($$),
      runResourceT,
      MonadResource )
import Data.Conduit.Audio as DCA
    ( framesToSeconds,
      integralSample,
      mapSamples,
      reorganize,
      splitChannels,
      takeStart,
      AudioSource(source, frames),
      Duration(Seconds),
      Frames )
import Data.Conduit.Audio.Sndfile
    ( sinkSnd, sourceSnd, sourceSndFrom )
import Data.Conduit.Audio.SampleRate
import qualified Sound.File.Sndfile as Snd
import Control.Concurrent ( threadDelay )
import Sound.VAD.WebRTC as Vad
    ( create, process, validRateAndFrameLength, VAD )
import qualified Data.Vector.Storable as V
import qualified Data.Conduit.List as CL
import Control.Monad.ST ( RealWorld )
import Data.Int (Int16)
import Control.Monad
import Control.Monad.State
    ( StateT(runStateT), MonadState(put, get) )
import Data.Maybe ( fromMaybe, isJust, isNothing )
import SendAudio ( sendAudio ) 
import Actions ( say, findResponseRegex )
import Data.Time.Clock ( UTCTime, getCurrentTime )
import System.Process

data ListenerST = ListenerST {
                                path :: FilePath,
                                startTime :: UTCTime,
                                timeOffset :: Double,
                                voiceStartTime :: Maybe Double,
                                voiceEndTime :: Maybe Double,
                                segmentDuration :: Double,
                                limit :: Double,
                                vad :: Vad.VAD RealWorld,
                                count :: Int
                              }
data RecordingBound = RecordingBound { voiceStart :: Maybe Double, voiceEnd :: Maybe Double }

timeChunk :: Double
timeChunk :: Double = 30.0 / 1000.0
audioRate :: Double
audioRate :: Double = 16000
thresholdPurportion :: Double
thresholdPurportion :: Double = 0.55 -- 55% of the samples should be on or off to turn on recording (splice out for recognition)
workingChunkSize :: Frames
workingChunkSize :: Frames = round $ audioRate * timeChunk

printSamples :: [V.Vector Int16] -> [Int]
printSamples   = map V.length 


detectVoice :: PrimMonad m => Vad.VAD (PrimState m) -> [V.Vector Int16] -> m [Bool]
detectVoice vd ss = let func s = Vad.process (round audioRate) s vd
                        validFrames = filter (\vs -> V.length vs == workingChunkSize) ss
                     in mapM func validFrames

myformat :: Snd.Format
myformat =  Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile


convertIntegral :: (MonadResource m) => AudioSource m Double -> AudioSource m Int16
convertIntegral  = DCA.mapSamples DCA.integralSample 

getwavs :: (MonadResource m, Snd.Sample a) => [FilePath] -> IO [AudioSource m a]
getwavs = mapM sourceSnd 

-- given a path, a start time and a duration (in seconds) will return:
-- a single channel audio source, 
-- with pulses recorded as Int16s
-- in chunks of 30 ms
getWavFrom :: (MonadResource m) => FilePath -> Double -> Double -> IO (AudioSource m Int16)
getWavFrom fp start dur = do src <- sourceSndFrom (Seconds start) fp
                             let split = DCA.splitChannels src -- we only want a single channel
                                 reorged = DCA.reorganize workingChunkSize (head split)
                                 timelimited = takeStart (Seconds dur) reorged
                             return $ convertIntegral timelimited

writeBoundedWave :: FilePath -> FilePath -> Double -> Double -> IO ()
writeBoundedWave oldPath newPath start end =
    do src <- getWavFrom oldPath start (end-start)
       runResourceT $ sinkSnd newPath myformat src


countTrue :: [Bool] -> Int
countTrue items = length $ filter (==True) items

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

getStartEnd :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
getStartEnd start end = do s <- start
                           e <- end
                           return (s,e)


calcDuration :: ListenerST -> Maybe Double
calcDuration listener = let se = getStartEnd (voiceEndTime listener) (voiceStartTime listener)
                            sub = uncurry (-)
                         in fmap sub se
 
debugPrint :: ListenerST -> IO ()
debugPrint listener = do print "------------------------------"
                         print "count:"
                         print (count listener)
                         print "time offset:"
                         print (timeOffset listener)
                         print "duration:"
                         print (segmentDuration listener)
                         print "voiceStart:"
                         print (voiceStartTime listener)
                         print "voiceEnd:"
                         print (voiceEndTime listener)
                         print "duration:"
                         print $ calcDuration listener

calcBoundary :: ListenerST -> [Bool] -> Double -> RecordingBound
calcBoundary listener activations elapsed = 
    let numberOn :: Int = countTrue activations
        sampleCount :: Int = length activations
        percentOn :: Double =  fromIntegral numberOn / fromIntegral sampleCount
        start = if (percentOn > thresholdPurportion) && isNothing (voiceStartTime listener)
            then Just (timeOffset listener)
            else voiceStartTime listener   --if there's already a value, don't overwrite it
        end = if isJust start && (percentOn < thresholdPurportion) && isNothing (voiceEndTime listener)
            then Just elapsed
            else voiceEndTime listener   -- if there's already a value, don't overwrite it
     in RecordingBound { voiceStart = start, voiceEnd = end}
 
isComplete :: RecordingBound -> Bool
isComplete bound = isJust (voiceStart bound) && isJust (voiceEnd bound)

resetVoiceBounds :: StateT ListenerST IO ()
resetVoiceBounds = do listener <- get
                      put listener { 
                                     voiceStartTime = Nothing, voiceEndTime = Nothing ,
                                      count = count listener + 1
                                   }
                      return ()
 
-- repeatedly reads from the capture file, looking for voice boundaries (start and stop)
-- when we find a voice boundary, we splice off that chunk of audio 
-- and send it to the voice recognition API (OpenAI whisper)
-- then we regex match the recognized voice text against possible known queries
-- and if it matches a known question, we generate an answer
-- and send it to the voice synthesis module

getWavST :: StateT ListenerST IO ()
getWavST = do listener <- get
              currentTime <- liftIO getCurrentTime
              src <- liftIO $ getWavFrom (path listener) (timeOffset listener) (segmentDuration listener)
              let length = DCA.framesToSeconds (frames src) audioRate
                  capfilepath = "tmp/capture" ++ show (count listener) ++ ".wav"
                  samples = DCA.source src
                  ending = timeOffset listener + length
                  elapsed = if length >0 then ending else ending +1
              ss <- liftIO $ runResourceT $ samples $$ sinkList --get samples from conduit
              voiceDetected :: [Bool] <- liftIO $ detectVoice (vad listener) ss --voice tagging
              let boundary = calcBoundary listener voiceDetected elapsed
              put listener { 
                              voiceStartTime = voiceStart boundary, 
                              voiceEndTime = voiceEnd boundary,
                              timeOffset = ending
                           }
              listener <- get
              if isComplete boundary && length >0
                then do liftIO $ do debugPrint listener
                                    let se = getStartEnd (voiceStart boundary) (voiceEnd boundary)
                                        (start,end) = fromMaybe (0.0,0.0) se  -- this default should never happen
                                    writeBoundedWave (path listener) capfilepath (start-0.5) end
                                    transcript <- sendAudio capfilepath
                                    response <- findResponseRegex transcript
                                    mapM_ say response
                        resetVoiceBounds
                        getWavST
                else liftIO $ threadDelay 1000000 -- sleep 1 second
              getWavST

initialState currentTime vad = ListenerST { 
  startTime = currentTime,
  path = "in.wav", 
  timeOffset=0.0, 
  segmentDuration=2.0, 
  limit=30.0, 
  vad=vad, 
  voiceStartTime = Nothing,
  voiceEndTime = Nothing,
  count = 0
}

main :: IO ()
main = do currentTime <- getCurrentTime
          print "vad-listener start"
          print currentTime
          vad <- Vad.create
          runStateT getWavST (initialState currentTime vad)
          return ()
