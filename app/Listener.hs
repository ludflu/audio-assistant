{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Listener where

import Numeric ( showFFloat )
import Sound.VAD.WebRTC as Vad
    ( VAD )

import Control.Monad.IO.Class
import Control.Monad.ST ( RealWorld )
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Control.Monad
import Control.Monad.State
    ( evalStateT, StateT(runStateT), MonadState(put, get), lift )


import Data.Maybe ( fromMaybe, isJust, isNothing )
import Data.Time.Clock ( UTCTime )
import System.Process

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
import SendAudio ( sendAudio ) 
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Char (isNumber)


data AppError = ReadAudioError | SystemError

data EnvConfig = EnvConfig { localpath :: FilePath }

data ListenerState = ListenerState {
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

newtype ListenerMonad a = ListenerMonad (StateT ListenerState IO a)
    deriving (Functor, Applicative, Monad, MonadIO )

data RecordingBound = RecordingBound { voiceStart :: Maybe Double, voiceEnd :: Maybe Double }

timeChunk :: Double = 30.0 / 1000.0
audioRate :: Double = 16000
thresholdPurportion :: Double = 0.55 -- 55% of the samples should be on or off to turn on recording (splice out for recognition)
workingChunkSize :: Frames = round $ audioRate * timeChunk


runListenerMonad :: ListenerMonad a -> ListenerState -> IO a
runListenerMonad  (ListenerMonad stateAction) listenerState = 
    evalStateT stateAction listenerState 
 
instance MonadState ListenerState ListenerMonad where
  get = ListenerMonad get
  put = ListenerMonad . put

initialState currentTime vad = ListenerState { 
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

getStartEnd :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
getStartEnd start end = do s <- start
                           e <- end
                           return (s,e)

calcDuration :: ListenerState -> Maybe Double
calcDuration listener = let se = getStartEnd (voiceEndTime listener) (voiceStartTime listener)
                            sub = uncurry (-)
                         in fmap sub se
 
speak :: a -> ListenerMonad a
speak v = return v 

debugPrint :: ListenerState -> IO ()
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

  
detectVoice :: PrimMonad m => Vad.VAD (PrimState m) -> [V.Vector Int16] -> m [Bool]
detectVoice vd ss = let func s = Vad.process (round audioRate) s vd
                        validFrames = filter (\vs -> V.length vs == workingChunkSize) ss
                     in mapM func validFrames

myformat :: Snd.Format
myformat =  Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

convertIntegral :: (MonadResource m) => AudioSource m Double -> AudioSource m Int16
convertIntegral  = DCA.mapSamples DCA.integralSample 

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

calcBoundary :: ListenerState -> [Bool] -> Double -> RecordingBound
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

resetVoiceBounds :: ListenerMonad ()
resetVoiceBounds = do listener <- get
                      put listener { 
                                     voiceStartTime = Nothing, voiceEndTime = Nothing ,
                                      count = count listener + 1
                                   }
                      return ()
 


dropNonNumbers :: String -> String
dropNonNumbers = filter (\x -> isNumber x)

parseInt :: String -> Maybe Integer
parseInt str = readMaybe $  dropNonNumbers str

listenForInteger :: ListenerMonad (Maybe Integer)
listenForInteger = do line <- listen
                      return $ parseInt line


-- repeatedly reads from the capture file, looking for voice boundaries (start and stop)
-- when we find a voice boundary, we splice off that chunk of audio 
-- and send it to the voice recognition API (OpenAI whisper)
-- then we return the string to the caller
listen :: ListenerMonad String
listen = do listener <- get
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
            if isComplete boundary && length >0
                then do resetVoiceBounds
                        liftIO $ do debugPrint listener
                                    let se = getStartEnd (voiceStart boundary) (voiceEnd boundary)
                                        (start,end) = fromMaybe (0.0,0.0) se  -- this default should never happen
                                    writeBoundedWave (path listener) capfilepath (start-0.5) end --back up half a second to make sure we don't lose anything
                                    transcript <- sendAudio capfilepath
                                    liftIO $ print "--------------------------"
                                    liftIO $ print transcript
                                    return transcript
                else do liftIO $ threadDelay 1000000 -- sleep 1 second
                        listen 


say :: String -> ListenerMonad String
say msg = let quoted = quote msg
              args = [quoted]
              emptystr :: String = ""
           in liftIO $ readProcess command args emptystr

trim :: String -> String
trim s = T.unpack $ T.strip $ T.pack s

quote :: String -> String
quote t = let q = "\"" 
           in q <> (trim t) <> q

command :: FilePath
command = "/home/jsnavely/project/vad-audio/scripts/talk.sh"


