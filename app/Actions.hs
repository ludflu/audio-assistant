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


command :: FilePath
command = "/home/jsnavely/project/audio-take-two/player/talk.sh"

currentTime :: IO String
currentTime = do time <- getCurrentTime
                 zone <- getCurrentTimeZone
                 let tt = show time
                     localTime = utcToLocalTime zone time
                     timeOfDay = localTimeOfDay localTime
                     hour = show $ todHour timeOfDay
                     minutes = show $ todMin timeOfDay
                     theTime = hour ++ " " ++ minutes
                 return theTime


responses :: M.Map String (IO String)
responses = M.fromList [ 
    ("hello computer", return "Hello Jim"),
    ("peace be with you", return "And also with you"),
    ("computer what time is it",  currentTime)
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
                 
