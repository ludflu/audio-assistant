{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import ChatLogger
import ConfigParser (EnvConfig (mailPassword, mailUser, ollamaHost, ollamaPort))
import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTVar)
import Control.Monad.Reader (MonadReader, ReaderT, ask, liftIO, runReaderT)
import Control.Monad.ST (RealWorld)
import Control.Monad.State
  ( MonadState (get, put),
    StateT (runStateT),
    evalStateT,
    gets,
    lift,
  )
import Data.Bits (Bits (xor))
import Data.Char (isLower, isNumber, isSpace, toLower)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.String.Conversions ()
import Data.Text (pack)
import Data.Time
import Data.Time.Calendar.WeekDate ()
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson ()
import Data.Time.LocalTime.TimeZone.Series
import Data.Traversable
import Guess (guessingGame)
import Listener (ListenerMonad, ListenerState (dbPool, mailbox), quitNow, readMail, say, speak, writeToMailBox)
import MatchHelper (dropNonLetters, fuzzyMatch, isMatch, lowerCase)
import OllamaApi (answerQuestion)
import RecordNote (readNote, recordNote)
import Reminders (setReminder)
import SayDateTime (currentDay, currentTime)
import SendEmail (email, sendEmailAnswer, sendEmailNote)
import System.Process
import Text.Regex.PCRE.Heavy (Regex, re, scan)
import WeatherFetcher (getWeather)

type ListenerAction = [String] -> ListenerMonad ()

greet :: [String] -> String
greet params = "Hello " ++ head params ++ " its nice to meet you"

acknowledgeAndAnswer :: [String] -> ListenerMonad ()
acknowledgeAndAnswer questions = do
  _ <- say "Thinking...  "
  env <- ask
  st <- get
  let question = head questions
  tstmp <- liftIO getCurrentTime
  qid <- liftIO $ addQuery (dbPool st) $ Query question tstmp
  OllamaApi.answerQuestion qid question

readLastAnswer :: ListenerMonad ()
readLastAnswer = do
  st <- get
  answers <- liftIO $ getAnswersForLastQuestion (dbPool st)
  let answerText = concat $ maybeToList answers
  mapM_ writeToMailBox answerText

regexResponses :: M.Map Regex ListenerAction
regexResponses =
  M.fromList
    [ ([re|computer my name is (.*)|], writeToMailBox . greet),
      ([re|computer what time is it|], const currentTime),
      ([re|computer please stop|], const quitNow),
      ([re|computer what day is it|], const currentDay),
      ([re|computer whats the weather|], const $ getWeather "key" "19038"),
      ([re|play the guessing game|], const guessingGame),
      ([re|record a note|], const recordNote),
      ([re|read the note|], const readNote),
      ([re|computer set a reminder for (.*) minutes|], setReminder),
      ([re|email the note|], const sendEmailNote),
      ([re|computer email the last answer|], const sendEmailAnswer),
      ([re|computer read the last answer|], const readLastAnswer),
      ([re|(?:okay|ok)[\,]? genius (.*)|], acknowledgeAndAnswer)
    ]

dispatchRegex :: M.Map Regex ListenerAction -> String -> ListenerMonad ()
dispatchRegex responses query =
  let q = dropNonLetters $ lowerCase query
      predicate (rgx, _) = isMatch q rgx
      candidates = M.toList responses
      maybeFunc = do
        (regx, respFunc) <- find predicate candidates
        let matches = fuzzyMatch q regx
            onlyMtchs = map snd matches
            onlyFst = map head onlyMtchs
        return $ respFunc onlyFst
   in fromMaybe (pure ()) maybeFunc

findResponseRegex :: String -> ListenerMonad ()
findResponseRegex = dispatchRegex regexResponses
