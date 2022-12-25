{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Numeric
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
import Control.Monad.State
import Data.Maybe

data AudioSegment = AudioSegment {
                                    segmentStart:: Double, 
                                    samples :: [V.Vector Int16], 
                                    isVoice :: V.Vector Bool
                                 } deriving Show

data ListenerST = ListenerST {
                                path :: FilePath,
                                startTime :: Double,
                                voiceStartTime :: Maybe Double,
                                voiceEndTime :: Maybe Double,
                                segmentDuration :: Double,
                                limit :: Double,
                                vad :: Vad.VAD RealWorld,
                                segments :: [AudioSegment]
                              }

timeChunk :: Double = 30.0 / 1000.0
audioRate :: Double = 16000
thresholdPurportion :: Double = 0.55 -- 55% of the samples should be on or off to turn on recording (splice out for recognition)
workingChunkSize :: Frames = round $ audioRate * timeChunk

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


writeWavMaybe :: FilePath -> FilePath -> Maybe Double -> Maybe Double -> IO ()
writeWavMaybe oldPath newPath start end =
    let startEnd = getStartEnd start end
     in case startEnd of 
        Nothing -> return ()
        Just (_start, _end) -> do src <- getWavFrom oldPath _start (_end-_start)
                                  runResourceT $ sinkSnd newPath myformat src


countOn :: [Bool] -> Int
countOn items = length $ filter (==True) items

countAll :: [Bool] -> Int
countAll items = length items


showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

getStartEnd :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
getStartEnd start end = do s <- start
                           e <- end
                           return (s,e)

getWavST :: StateT ListenerST IO ()
getWavST = do listener <- get
              src <- liftIO $ getWavFrom (path listener) (startTime listener) (segmentDuration listener)
              let length = DCA.framesToSeconds (frames src) audioRate
                  samples = DCA.source src
                  threshold = round $ thresholdPurportion * (segmentDuration listener)
                  ending = (startTime listener) + length
                  elapsed = if (length >0) then ending else ending +1
                  newpath = "./tmp/file" ++ show ending ++ ".wav"
              ss <- liftIO $ runResourceT $ samples $$ sinkList --get samples from conduit
              voiceDetected :: [Bool] <- liftIO $ runSamples (vad listener) ss --voice tagging
              let numberOn :: Int = countOn voiceDetected
                  sampleCount :: Int = countAll voiceDetected
                  percentOn :: Double =  fromIntegral numberOn / fromIntegral sampleCount
                  percentOff = 1.0 - percentOn
                  start = if (percentOn > thresholdPurportion) && (isNothing $ voiceStartTime listener)
                             then Just (startTime listener)
                             else voiceStartTime listener   --if there's already a value, don't overwrite it
                  end = if (isJust start) && (percentOn < thresholdPurportion) && (isNothing $ voiceEndTime listener)
                             then Just elapsed
                             else voiceEndTime listener   -- if there's already a value, don't overwrite it
                  segs = AudioSegment { 
                                        segmentStart=(startTime listener), 
                                        samples = ss, 
                                        isVoice=V.fromList voiceDetected
                                      }
              _ <- liftIO $ putStrLn "-----------------------"
              _ <- liftIO $ putStrLn $ showFullPrecision percentOn
              _ <- liftIO $ putStrLn $ showFullPrecision percentOff
              _ <- liftIO $ putStrLn $ "elapsed:"
              _ <- liftIO $ putStrLn $ show elapsed
              _ <- liftIO $ putStrLn $ show start
              _ <- liftIO $ putStrLn $ show end

              put listener { startTime = elapsed, 
                             segments = segs : (segments listener),
                             voiceStartTime = start,
                             voiceEndTime = end
                           }
              if (isJust start && isJust end) || elapsed > limit listener
                then liftIO $ writeWavMaybe (path listener) "tmp/captured.wav" start end
                else getWavST


isValidRateAndFrame = Vad.validRateAndFrameLength (round audioRate) workingChunkSize

main :: IO ()
main = do vad <- Vad.create
          let initialState = ListenerST { 
              path = "in.wav", 
              startTime=0.0, 
              segmentDuration=2.0, 
              limit=30.0, 
              vad=vad, 
              segments = [],
              voiceStartTime = Nothing,
              voiceEndTime = Nothing
            }

          (se, ls) <- runStateT getWavST initialState  
          _ <- putStrLn  "---------------------------------------------------"
          _ <- putStrLn $ show (voiceStartTime ls)
          _ <- putStrLn $ show (voiceEndTime ls)

          return ()
