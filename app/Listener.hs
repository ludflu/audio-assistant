{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Listener where

import ConfigParser (EnvConfig (recordingLength, sileroHost, sileroPort), activationThreshold, audioRate, debug, localpath, segmentDuration, sleepSeconds, wavpath, whisperHost, whisperPort)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, tryTakeMVar)
import Control.Concurrent.STM (STM, TQueue, atomically, readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad (when)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Loops (untilM_)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.ST (RealWorld)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    gets,
    lift,
  )
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Numeric (showFFloat)
import SendAudio (sendAudio)
import Sound.VAD.WebRTC as Vad
  ( VAD,
  )
import SpeechApi
import System.Directory (doesFileExist)
import System.Process (readProcess)
import VoiceDetectionSliceReader (readSlice, writeBoundedWave)

data ListenerState = ListenerState
  { path :: FilePath,
    startTime :: Data.Time.Clock.UTCTime,
    timeOffset :: Double,
    voiceStartTime :: Maybe Double,
    voiceEndTime :: Maybe Double,
    vad :: Vad.VAD RealWorld,
    count :: Int,
    quit :: Bool,
    audioReset :: MVar FilePath,
    mailbox :: TQueue String
  }

newtype ListenerMonad a = ListenerMonad (ReaderT EnvConfig (StateT ListenerState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader EnvConfig)

data RecordingBound = RecordingBound {voiceStart :: Maybe Double, voiceEnd :: Maybe Double}

runListenerMonad :: ListenerMonad a -> EnvConfig -> ListenerState -> IO a
runListenerMonad (ListenerMonad stateAction) envConfig =
  evalStateT (runReaderT stateAction envConfig)

instance MonadState ListenerState ListenerMonad where
  get :: ListenerMonad ListenerState
  get = ListenerMonad get
  put :: ListenerState -> ListenerMonad ()
  put = ListenerMonad . put

initialState :: Data.Time.Clock.UTCTime -> VAD RealWorld -> MVar FilePath -> TQueue String -> FilePath -> ListenerState
initialState currentTime vad wasAudioReset mailbox initialPath =
  ListenerState
    { startTime = currentTime,
      path = initialPath,
      timeOffset = 0.0,
      vad = vad,
      voiceStartTime = Nothing,
      voiceEndTime = Nothing,
      count = 0,
      quit = False,
      audioReset = wasAudioReset,
      mailbox = mailbox
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

waitForFileToArrive :: FilePath -> IO ()
waitForFileToArrive filename =
  untilM_
    (threadDelay 100000)
    (doesFileExist filename)

getListenerState :: ListenerMonad ListenerState
getListenerState = do
  listener <- get
  wasRecordingReset <- liftIO $ tryTakeMVar $ audioReset listener
  mail <- liftIO $ atomically $ tryReadTQueue $ mailbox listener

  when (isJust mail) (mapM_ say mail)
  resetOffset wasRecordingReset
  get

listenPatiently :: ListenerMonad String
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

  when (timeOffset listener > fromIntegral (recordingLength env)) (liftIO $ print "max time exceeded")
  when (debug env) (liftIO $ debugPrint listener)
  (voiceActivations, length) <- liftIO $ readSlice (path listener) (timeOffset listener) (segmentDuration env) (audioRate env) (vad listener)
  let ending = timeOffset listener + length
      elapsed = if length > 0 then ending else ending + 1
      boundary = calcBoundary listener voiceActivations elapsed threshold
      capfilepath =
        if null $ wavpath env
          then localpath env ++ "/capture" ++ show (count listener) ++ ".wav"
          else wavpath env ++ "/capture" ++ show (count listener) ++ ".wav"
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
        transcript <- sendAudio ("http://" ++ whisperHost env ++ "/") (whisperPort env) capfilepath
        when (debug env) (liftIO $ print transcript)
        return transcript
    else do
      liftIO $ threadDelay $ round (sleepSeconds env * 1000000)
      listen

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

-- this function is called when a new recording starts
resetOffset :: Maybe FilePath -> ListenerMonad ()
resetOffset newpath =
  case newpath of
    Just fp -> do
      liftIO $ threadDelay 1500000 -- wait 1.5 seconds for new recording to be available
      liftIO $ print "new audio file!"
      liftIO $ print fp
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
  gets quit

writeToMailBox :: String -> ListenerMonad ()
writeToMailBox msg =
  do
    s <- get
    liftIO $ print msg
    liftIO $ atomically $ writeTQueue (mailbox s) msg

say :: String -> ListenerMonad Double
say msg =
  do
    startTime <- liftIO getCurrentTime
    listenerState <- get
    config <- ask
    dur <- liftIO $ sayText (sileroHost config) (sileroPort config) msg
    endTime <- liftIO getCurrentTime
    let offset = timeOffset listenerState + dur + sleepSeconds config -- advance the offset to skip over the time when the computer was talking
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
