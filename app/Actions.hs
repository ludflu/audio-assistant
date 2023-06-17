{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Actions where

import Data.List
import System.Process
import qualified Data.Map as M
import Data.Maybe

import Data.Time.Clock ( UTCTime, getCurrentTime )
import Data.Time.LocalTime

import Data.Char ( isSpace, isLower, toLower)

import SayDateTime

import Data.Time                             -- package "time"
import Data.Time.Calendar.WeekDate           -- package "time"
import Data.Time.LocalTime.TimeZone.Olson    -- package "timezone-olson"
import Data.Time.LocalTime.TimeZone.Series
import Text.Regex.PCRE.Heavy
import Data.String.Conversions
import Data.Traversable
import Listener
import Guess
import RecordNote

import WeatherFetcher

greet :: [String] -> String
greet params = "Hello " ++ head params ++ " its nice to meet you"

regexResponses :: M.Map Regex ([String] -> ListenerMonad String)
regexResponses = M.fromList [ 
    ( [re|computer my name is (.*)|] , speak . greet ),
    ( [re|computer what time is it|],  const currentTime),
    ( [re|computer please stop|],  \x -> quitNow >> speak "Goodbye."),
    ( [re|computer what day is it|],  const currentDay),
    ( [re|computer whats the weather|],  const $ getWeather "key" "19038" ),
    ( [re|play the guessing game|],  const guessingGame),
    ( [re|record a note|],  const recordNote),
    ( [re|read note|],  const readNote),
    ( [re|i love you computer|], \x -> speak "I love you too!")
                            ]

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

dropNonLetters :: String -> String
dropNonLetters = filter (\x -> isLower x || isSpace x)

-- isYes :: String -> Bool
-- isYes response = let r' = map toUpper response in
--                      isMatch response [re|YES|AFFIRMATIVE|]

-- isMatch :: String -> Regex -> Bool
-- isMatch s r = not (null (scan r s))

fuzzyMatch :: String -> Regex -> [ (String, [String])]
fuzzyMatch s r = scan r s

dispatchRegex :: M.Map Regex ([String] -> ListenerMonad String) -> String -> Maybe (ListenerMonad String)
dispatchRegex responses query = let q = dropNonLetters $ lowerCase query
                                    predicate (rgx,_)  = isMatch q rgx
                                    candidates = M.toList responses
                                    maybeFunc = do (regx,respFunc) <- find predicate candidates
                                                   let matches = fuzzyMatch q regx
                                                       onlyMtchs = map snd matches
                                                       onlyFst = map head onlyMtchs
                                                   return $ respFunc onlyFst
                                 in maybeFunc 




findResponseRegex :: String -> ListenerMonad (Maybe String)
findResponseRegex query = sequence $ dispatchRegex regexResponses query
