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
import qualified Data.Text as T
import Text.Regex.PCRE.Heavy
import Data.String.Conversions
import Data.Traversable

command :: FilePath
command = "/home/jsnavely/project/vad-audio/talk.sh"

greet :: [String] -> String
greet params = "Hello " ++ head params ++ " its nice to meet you"

regexResponses :: M.Map Regex ([String] -> IO String)
regexResponses = M.fromList [ 
    ( [re|computer my name is (.*)|] , \x -> return $ greet x),
    ( [re|computer what time is it|],  \x -> currentTime),
    ( [re|computer what day is it|],  \x -> currentDay),
    ( [re|i love you computer|], \x -> return "I love you too!")
                            ]

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

trim :: String -> String
trim s = T.unpack $ T.strip $ T.pack s

quote :: String -> String
quote t = let q = "\"" 
           in q <> (trim t) <> q

say :: String -> IO String
say msg = let quoted = quote msg
              args = [quoted]
              emptystr :: String = ""
           in readProcess command args emptystr

sayHello :: String -> IO String
sayHello name = say $ "hello there " ++ name

dropNonLetters :: String -> String
dropNonLetters = filter (\x -> isLower x || isSpace x)


isMatch :: String -> Regex -> Bool
isMatch s r = (length (scan r s)) > 0

fuzzyMatch :: String -> Regex -> [ (String, [String])]
fuzzyMatch s r = scan r s

dispatchRegex :: M.Map Regex ([String] -> IO String) -> String -> Maybe (IO String)
dispatchRegex responses query = let q = dropNonLetters $ lowerCase query
                                    predicate (rgx,_)  = isMatch q rgx
                                    candidates = M.toList responses
                                    maybeFunc = do (regx,respFunc) <- find predicate candidates
                                                   let matches = fuzzyMatch q regx
                                                       onlyMtchs = map snd matches
                                                       onlyFst = map head onlyMtchs
                                                   return $ respFunc onlyFst
                                 in maybeFunc 

findResponseRegex :: String -> IO (Maybe String)
findResponseRegex query = sequence $ dispatchRegex regexResponses query
