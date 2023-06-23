{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Listener where

import Conduit
  ( MonadIO (liftIO),
    MonadResource,
    PrimMonad (PrimState),
    runResourceT,
    sinkList,
    ($$),
  )
import ConfigParser (EnvConfig, activationThreshold, audioRate, debug, localpath, segmentDuration, sleepSeconds, wavpath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, tryTakeMVar)
import Control.Monad (when)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.ST (RealWorld)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    lift,
  )
import Data.Char (isNumber, toLower)
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
import qualified Data.Conduit.List as CL
import Data.Int (Int16)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Data.Vector.Storable as V
import Numeric (showFFloat)
import SendAudio (sendAudio)
import qualified Sound.File.Sndfile as Snd
import Sound.VAD.WebRTC as Vad
  ( VAD,
    create,
    process,
    validRateAndFrameLength,
  )
import SpeechApi (sayText)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Text.Regex.PCRE.Heavy (Regex, re, scan)

data ListenerState = ListenerState
  { path :: FilePath,
    startTime :: Data.Time.Clock.UTCTime,
    timeOffset :: Double,
    voiceStartTime :: Maybe Double,
    voiceEndTime :: Maybe Double,
    vad :: Vad.VAD RealWorld,
    count :: Int,
    quit :: Bool,
    audioReset :: MVar FilePath
  }

newtype ListenerMonad a = ListenerMonad (ReaderT EnvConfig (StateT ListenerState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader EnvConfig)

data RecordingBound = RecordingBound {voiceStart :: Maybe Double, voiceEnd :: Maybe Double}

timeChunk :: Double = 30.0 / 1000.0

workingChunkSize :: Double -> Frames
workingChunkSize rate = round $ rate * timeChunk

runListenerMonad :: ListenerMonad a -> EnvConfig -> ListenerState -> IO a
runListenerMonad (ListenerMonad stateAction) envConfig =
  evalStateT (runReaderT stateAction envConfig)

instance MonadState ListenerState ListenerMonad where
  get = ListenerMonad get
  put = ListenerMonad . put

initialState :: Data.Time.Clock.UTCTime -> VAD RealWorld -> MVar String -> FilePath -> ListenerState
initialState currentTime vad wasAudioReset initialPath =
  ListenerState
    { startTime = currentTime,
      path = initialPath,
      timeOffset = 0.0,
      vad = vad,
      voiceStartTime = Nothing,
      voiceEndTime = Nothing,
      count = 0,
      quit = False,
      audioReset = wasAudioReset
    }

getStartEnd :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
getStartEnd start end = do
  s <- start
  e <- end
  return (s, e)

calcDuration :: ListenerState -> Maybe Double
calcDuration listener =
  let se = getStartEnd (voiceEndTime listener) (voiceStartTime listener)
      sub = uncurry (-)
   in fmap sub se

speak :: a -> ListenerMonad a
speak = return

debugPrint :: ListenerState -> IO ()
debugPrint listener =
  do
    print "------------------------------"
    print "count:"
    print (count listener)
    print "time offset:"
    print (timeOffset listener)
    print "voiceStart:"
    print (voiceStartTime listener)
    print "voiceEnd:"
    print (voiceEndTime listener)
    print "duration:"
    print $ calcDuration listener
    print "path:"
    print $ path listener

detectVoice :: PrimMonad m => Vad.VAD (PrimState m) -> [V.Vector Int16] -> Double -> m [Bool]
detectVoice vd ss rate =
  let func s = Vad.process (round rate) s vd
      validFrames = filter (\vs -> V.length vs == workingChunkSize rate) ss
   in mapM func validFrames

myformat :: Snd.Format
myformat = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

convertIntegral :: (MonadResource m) => AudioSource m Double -> AudioSource m Int16
convertIntegral = DCA.mapSamples DCA.integralSample

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

countTrue :: [Bool] -> Int
countTrue items = length $ filter id items

calcBoundary :: ListenerState -> [Bool] -> Double -> Double -> RecordingBound
calcBoundary listener activations elapsed thresholdPurportion =
  let numberOn :: Int = countTrue activations
      sampleCount :: Int = length activations
      numberOff :: Int = sampleCount - numberOn
      percentOn :: Double = fromIntegral numberOn / fromIntegral sampleCount
      percentOff :: Double = fromIntegral numberOff / fromIntegral sampleCount
      start =
        if (percentOn > thresholdPurportion) && isNothing (voiceStartTime listener)
          then Just (timeOffset listener)
          else voiceStartTime listener -- if there's already a value, don't overwrite it
      end =
        if isJust start && (percentOff > thresholdPurportion) && isNothing (voiceEndTime listener)
          then Just elapsed
          else voiceEndTime listener -- if there's already a value, don't overwrite it
   in RecordingBound {voiceStart = start, voiceEnd = end}

isComplete :: RecordingBound -> Bool
isComplete bound = isJust (voiceStart bound) && isJust (voiceEnd bound)

resetVoiceBounds :: ListenerMonad ()
resetVoiceBounds = do
  listener <- get
  put
    listener
      { voiceStartTime = Nothing,
        voiceEndTime = Nothing,
        count = count listener + 1
      }
  return ()

dropNonNumbers :: String -> String
dropNonNumbers = filter isNumber

parseInt :: String -> Maybe Integer
parseInt str = readMaybe $ dropNonNumbers str

listenForInteger :: ListenerMonad (Maybe Integer)
listenForInteger = parseInt <$> listen

getListenerState :: ListenerMonad ListenerState
getListenerState = do
  listener <- get
  wasRecordingReset <- liftIO $ tryTakeMVar $ audioReset listener
  resetOffset wasRecordingReset
  get

isYes :: String -> Bool
isYes response =
  let r' = map toLower response
   in isMatch r' [re|yes|affirmative|]

isNo :: String -> Bool
isNo response =
  let r' = map toLower response
   in isMatch r' [re|no|negative|]

isMatch :: String -> Regex -> Bool
isMatch s r = not (null (scan r s))

listenYesNo :: ListenerMonad Bool
listenYesNo = do isYes <$> listenPatiently

askQuestion :: String -> ListenerMonad Bool
askQuestion q = do
  say q
  a <- listenPatiently
  let yes = isYes a
      no = isNo a
  if yes
    then return True
    else
      if no
        then return False
        else say "Please say 'yes, affirmative' or 'no, negative'" >> askQuestion q

listenPatiently = listenWithThreshold 0.70

listen :: ListenerMonad String
listen = do
  env <- ask
  listenWithThreshold (activationThreshold env)

-- repeatedly reads from the capture file, looking for voice boundaries (start and stop)
-- when we find a voice boundary, we splice off that chunk of audio
-- and send it to the voice recognition API (OpenAI whisper)
-- then we return the string to the caller
listenWithThreshold :: Double -> ListenerMonad String
listenWithThreshold threshold = do
  listener <- getListenerState
  env <- ask
  when (debug env) (liftIO $ debugPrint listener)
  src <- liftIO $ getWavFrom (path listener) (timeOffset listener) (segmentDuration env) (audioRate env)
  let length = DCA.framesToSeconds (frames src) (audioRate env)
      capfilepath =
        if null $ wavpath env
          then localpath env ++ "/capture" ++ show (count listener) ++ ".wav"
          else wavpath env ++ "/capture" ++ show (count listener) ++ ".wav"
      samples = DCA.source src
      ending = timeOffset listener + length
      elapsed = if length > 0 then ending else ending + 1
  ss <- liftIO $ runResourceT $ samples $$ sinkList -- get samples from conduit
  voiceDetected :: [Bool] <- liftIO $ detectVoice (vad listener) ss (audioRate env) -- voice tagging
  let boundary = calcBoundary listener voiceDetected elapsed threshold
  put
    listener
      { voiceStartTime = voiceStart boundary,
        voiceEndTime = voiceEnd boundary,
        timeOffset = ending
      }
  if isComplete boundary && length > 0
    then do
      resetVoiceBounds
      liftIO $ do
        let se = getStartEnd (voiceStart boundary) (voiceEnd boundary)
            (start, end) = fromMaybe (0.0, 0.0) se -- this default should never happen
        writeBoundedWave (path listener) capfilepath (start - 0.25) end (audioRate env) -- back up a 1/4 second to make sure we don't lose anything
        transcript <- sendAudio capfilepath
        when (debug env) (liftIO $ print transcript)
        return transcript
    else do
      liftIO $ threadDelay $ round (sleepSeconds env * 1000000)
      listen

resetOffset :: Maybe FilePath -> ListenerMonad ()
resetOffset newpath =
  case newpath of
    Just fp -> do
      currentTime <- liftIO getCurrentTime
      listenerState <- get
      put
        listenerState
          { path = fp,
            timeOffset = 0.0,
            voiceStartTime = Nothing,
            voiceEndTime = Nothing,
            startTime = currentTime
          }
      return ()
    Nothing -> return ()

quitNow :: ListenerMonad ()
quitNow = do
  listener <- get
  put
    listener
      { quit = True
      }
  return ()

shouldQuit :: ListenerMonad Bool
shouldQuit =
  quit <$> get

say :: String -> ListenerMonad Double
say msg =
  let quoted = quote msg
      args = [quoted]
      emptystr :: String = ""
   in do
        startTime <- liftIO getCurrentTime
        listenerState <- get
        config <- ask
        liftIO $ readProcess (command config) args emptystr
        endTime <- liftIO getCurrentTime
        let dur = realToFrac $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime
        let offset = timeOffset listenerState + dur + 0.50
        put
          listenerState
            { timeOffset = offset,
              voiceStartTime = Nothing,
              voiceEndTime = Nothing
            }
        return dur

trim :: String -> String
trim s = T.unpack $ T.strip $ T.pack s

quote :: String -> String
quote t =
  let q = "\""
   in q <> trim t <> q

command :: EnvConfig -> FilePath
command config = localpath config ++ "/scripts/talk.sh"
