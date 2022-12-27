{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actions where

import Data.List
import System.Process
import qualified Data.Map as M
import Data.Maybe

import Data.Char (toUpper)

command = "/home/jsnavely/project/audio-take-two/player/talk.sh"

responses = M.fromList [ 
    ("hello computer", "Hello Jim"),
    ("peace be with you","And also with you")
                       ]
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

fuzzyMatch :: String -> String -> Bool
fuzzyMatch haystack needle = let h = capitalise haystack
                                 n = capitalise needle
                              in isInfixOf n h

dispatch :: M.Map String String -> String -> Maybe String
dispatch responses query = let predicate = \(a,b) -> fuzzyMatch query a
                               candidates = M.toList responses 
                               match = find predicate candidates
                            in fmap snd match

findResponse :: String -> Maybe String
findResponse q = dispatch responses q
