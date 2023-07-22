{-# LANGUAGE QuasiQuotes #-}

module MatchHelper where

import Data.Char (isNumber, toLower)
import Text.Read (readMaybe)
import Text.Regex.PCRE.Heavy (Regex, re, scan)

isYes :: String -> Bool
isYes response =
  let r' = map toLower response
   in isMatch r' [re|yes|affirmative|]

isNo :: String -> Bool
isNo response =
  let r' = map toLower response
   in isMatch r' [re|no|negative|]

isMatch :: String -> Regex -> Bool
isMatch s r = not (null (scan r s))

dropNonNumbers :: String -> String
dropNonNumbers = filter isNumber

parseInt :: String -> Maybe Integer
parseInt str = readMaybe $ dropNonNumbers str
