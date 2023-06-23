{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import Data.Char (isLower, isSpace, toLower)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.String.Conversions
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series
import Data.Traversable
import Guess
import Listener
import MatchHelper
import RecordNote
import SayDateTime
import System.Process
import Text.Regex.PCRE.Heavy
import WeatherFetcher

greet :: [String] -> String
greet params = "Hello " ++ head params ++ " its nice to meet you"

regexResponses :: M.Map Regex ([String] -> ListenerMonad String)
regexResponses =
  M.fromList
    [ ([re|computer my name is (.*)|], speak . greet),
      ([re|computer what time is it|], const currentTime),
      ([re|computer please stop|], \x -> quitNow >> speak "Goodbye."),
      ([re|computer what day is it|], const currentDay),
      ([re|computer whats the weather|], const $ getWeather "key" "19038"),
      ([re|play the guessing game|], const guessingGame),
      ([re|record a note|], const recordNote),
      ([re|read note|], const readNote),
      ([re|i love you computer|], \x -> speak "I love you too!")
    ]

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

dropNonLetters :: String -> String
dropNonLetters = filter (\x -> isLower x || isSpace x)

fuzzyMatch :: String -> Regex -> [(String, [String])]
fuzzyMatch s r = scan r s

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
