{-# LANGUAGE QuasiQuotes #-}

module MatchHelper where

import Control.Applicative ((<|>))
import Data.Char (isNumber, toLower)
import qualified Data.Map as M
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

nums :: [String]
nums = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]

teens :: [String]
teens = ["", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred"]

numsToText :: Int -> String
numsToText i
  | i <= 20 = nums !! i
  | i < 100 = teens !! ((i `div` 10) - 1) ++ " " ++ nums !! (i `mod` 10)

numsToText' :: Integer -> String
numsToText' i =
  let ii = fromInteger i
   in numsToText ii

nummap :: M.Map String Integer
nummap =
  let numnum = map toInteger [1 .. 100]
      numstrs = map numsToText' numnum
   in M.fromList (zip numstrs numnum)

lookupNum :: String -> Maybe Integer
lookupNum i = M.lookup i nummap

parseInt :: String -> Maybe Integer
parseInt str = readMaybe $ dropNonNumbers str

readInt :: String -> Maybe Integer
readInt str = parseInt str <|> lookupNum str