{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import Data.List
import System.Process
import qualified Data.Map as M
import Data.Maybe

import Data.Time.Clock ( UTCTime, getCurrentTime )
import Data.Time.LocalTime

import Data.Char ( isSpace, isUpper, toUpper )


import Data.Time                             -- package "time"
import Data.Time.Calendar.WeekDate           -- package "time"
import Data.Time.LocalTime.TimeZone.Olson    -- package "timezone-olson"
import Data.Time.LocalTime.TimeZone.Series

command :: FilePath
command = "/home/jsnavely/project/audio-take-two/player/talk.sh"

getLocalTime = do time <- getCurrentTime
                  zone <- getCurrentTimeZone
                  return $ utcToLocalTime zone time


monthMap :: M.Map Int String 
monthMap = M.fromList [ 
                        (1,"January"), 
                        (2,"February"), 
                        (3,"March"),
                        (4,"April"),
                        (5,"May"),
                        (6,"June"),
                        (7,"July"),
                        (8,"August"),
                        (9,"September"),
                        (10,"October"),
                        (11,"November"),
                        (12,"December")
                     ]

getMonthString :: Int -> String
getMonthString month = let mstr = M.lookup month monthMap
                        in fromMaybe "" mstr

currentDay :: IO String
currentDay = do localTime <- getLocalTime
                let LocalTime day (TimeOfDay hh mm ss) = localTime
                    (yr, mn, dom) = toGregorian day
                    (_,  wk, dow) = toWeekDate day
                    formatDay :: String =  getMonthString mn ++ " " ++ show dom ++ ", " ++ show yr 
                return formatDay

currentTime :: IO String
currentTime = do localTime <- getLocalTime
                 let timeOfDay = localTimeOfDay localTime
                     hour = show $ (todHour timeOfDay) `mod` 12
                     minutes = show $ todMin timeOfDay
                     theTime = hour ++ " " ++ minutes
                 return theTime


responses :: M.Map String (IO String)
responses = M.fromList [ 
    ("hello computer", return "Hello Jim"),
    ("peace be with you", return "And also with you"),
    ("computer what time is it",  currentTime),
    ("computer what day is it",  currentDay)
                       ]
capitalise :: [Char] -> [Char]
capitalise = map toUpper

quote :: String -> String
quote t = let q = "\"" 
           in q <> t <> q

say :: String -> IO String
say msg = let quoted = quote msg
              args = [quoted]
              emptystr :: String = ""
           in readProcess command args emptystr

sayHello :: String -> IO String
sayHello name = say $ "hello there " ++ name

dropNonLetters :: String -> String
dropNonLetters = filter (\x -> isUpper x || isSpace x)

fuzzyMatch :: String -> String -> Bool
fuzzyMatch haystack needle = let h = capitalise haystack
                                 n = capitalise needle
                                 hh = dropNonLetters h
                              in isInfixOf n hh

dispatch :: M.Map String (IO String) -> String -> Maybe (IO String)
dispatch responses query = let predicate (a,b) = fuzzyMatch query a
                               candidates = M.toList responses 
                               match = find predicate candidates
                            in fmap snd match

findResponse :: String -> Maybe (IO String)
findResponse = dispatch responses 
                 
