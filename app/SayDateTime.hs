{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SayDateTime where

import Data.List
import qualified Data.Map as M
import Data.Maybe

import Data.Time.Clock ( UTCTime, getCurrentTime )
import Data.Time.LocalTime


import Data.Time                             -- package "time"
import Data.Time.Calendar.WeekDate           -- package "time"
import Data.Time.LocalTime.TimeZone.Olson    -- package "timezone-olson"
import Data.Time.LocalTime.TimeZone.Series

import Listener ( ListenerMonad, speak ) 

import Control.Monad.State (liftIO)
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

currentDay :: ListenerMonad String
currentDay = do localTime <- liftIO getLocalTime
                let LocalTime day (TimeOfDay hh mm ss) = localTime
                    (yr, mn, dom) = toGregorian day
                    (_,  wk, dow) = toWeekDate day
                    formatDay :: String =  getMonthString mn ++ " " ++ show dom ++ ", " ++ show yr 
                speak $ "Today is " ++ formatDay

currentTime :: ListenerMonad String
currentTime = do localTime <- liftIO getLocalTime
                 let timeOfDay = localTimeOfDay localTime
                     hour = show $ todHour timeOfDay `mod` 12
                     minutes = show $ todMin timeOfDay
                     theTime = hour ++ ":" ++ minutes
                 speak $ "The time is " ++ theTime

