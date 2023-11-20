{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import ConfigParser (EnvConfig (mailPassword, mailUser))
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
import Listener (ListenerMonad, quitNow, speak)
import MatchHelper (dropNonLetters, fuzzyMatch, isMatch, lowerCase)
import OllamaApi (answerQuestion)
import RecordNote (readNote, recordNote)
import Reminders (setReminder)
import SayDateTime (currentDay, currentTime)
import SendEmail (email, sendEmailNote)
import System.Process
import Text.Regex.PCRE.Heavy (Regex, re, scan)
import WeatherFetcher (getWeather)

type ListenerAction = [String] -> ListenerMonad String

greet :: [String] -> String
greet params = "Hello " ++ head params ++ " its nice to meet you"

askAndAcknowledge :: String -> ListenerMonad String
askAndAcknowledge question = do
  liftIO $ OllamaApi.answerQuestion question

regexResponses :: M.Map Regex ListenerAction
regexResponses =
  M.fromList
    [ ([re|computer my name is (.*)|], speak . greet),
      ([re|computer what time is it|], const currentTime),
      ([re|computer please stop|], \x -> quitNow >> speak "Goodbye."),
      ([re|computer what day is it|], const currentDay),
      ([re|computer whats the weather|], const $ getWeather "key" "19038"),
      ([re|play the guessing game|], const guessingGame),
      ([re|record a note|], const recordNote),
      ([re|read the note|], const readNote),
      ([re|computer set a reminder for (.*) minutes|], setReminder),
      ([re|email the note|], const sendEmailNote),
      ([re|i love you computer|], \x -> speak "I love you too!"),
      ([re|okay genius (.*)|], \x -> speak "Thinking!" >> (askAndAcknowledge $ head x))
    ]

dispatchRegex :: M.Map Regex ([String] -> ListenerMonad String) -> String -> Maybe (ListenerMonad String)
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
   in maybeFunc

findResponseRegex :: String -> ListenerMonad (Maybe String)
findResponseRegex query = sequence $ dispatchRegex regexResponses query
