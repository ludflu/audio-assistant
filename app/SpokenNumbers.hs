{-# LANGUAGE QuasiQuotes #-}

module SpokenNumbers (convertNumber, convertNString, convertAllNumbers, readUnit, numLookup) where

import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Numerals
import Text.Regex.PCRE.Heavy (Regex, gsub, re)

-- convertNumber :: Integer -> Text
-- convertNumber n = toCardinal english n

convertNumber :: Integer -> String
convertNumber n = T.unpack $ toCardinal english n

removeCommas :: String -> String
removeCommas = filter (/= ',')

type Unit = String

readUnit :: String -> Maybe Int
readUnit s = case reads s of -- the integer is at the beginning of the string and...
  (n, ' ' : unit) : _ -> Just n -- is followed by space...
  (n, "") : _ -> Just n -- or nothing.
  _ -> Nothing

convertNString :: String -> String
convertNString nstr =
  let i = readUnit $ removeCommas nstr
   in case i of
        Just n -> convertNumber (toInteger n)
        Nothing -> nstr

convertAllNumbers :: String -> String
convertAllNumbers = gsub [re|([0-9,]+)|] convertNString

punctToSpace :: String -> String
punctToSpace str = gsub [re|([^a-z,]+)|] " " str

makeLower :: String -> String
makeLower = map toLower

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

nummap :: M.Map String Integer
nummap =
  let nm = map convertNumber [0 .. 100]
      nmLower = map makeLower nm
      regnums = map punctToSpace nmLower
   in M.fromList $ zip regnums [0 .. 100]

lookupNum :: String -> Maybe Integer
lookupNum s =
  let needle = trim $ punctToSpace $ makeLower s
   in M.lookup needle nummap