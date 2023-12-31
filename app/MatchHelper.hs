{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MatchHelper where

import Control.Applicative ((<|>))
import Data.Char (isLower, isNumber, isSpace, toLower)
import qualified Data.Map as M
import qualified Data.Text as T
import SpokenNumbers
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

strip :: String -> String
strip s = T.unpack $ T.strip (T.pack s) -- I know this shouldn't be required due to OverloadedStrings, and yet

parseInt :: String -> Maybe Integer
parseInt str = readMaybe $ dropNonNumbers str

readInt :: String -> Maybe Integer
readInt str = parseInt str <|> lookupNum str

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

dropNonLetters :: String -> String
dropNonLetters = filter (\x -> isLower x || isSpace x || isNumber x)

fuzzyMatch :: String -> Regex -> [(String, [String])]
fuzzyMatch s r = scan r s
