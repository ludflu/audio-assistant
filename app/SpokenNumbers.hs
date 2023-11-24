{-# LANGUAGE QuasiQuotes #-}

module SpokenNumbers (convertNumber, convertNString, convertAllNumbers, readUnit) where

import qualified Data.Text as T
import Text.Numerals
import Text.Regex.PCRE.Heavy (Regex, gsub, re)

-- convertNumber :: Integer -> Text
-- convertNumber n = toCardinal english n

convertNumber :: Integer -> String
convertNumber n = T.unpack $ toCardinal english n

removeCommas :: String -> String
removeCommas s = filter (\x -> x /= ',') s

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
convertAllNumbers = gsub [re|([0-9,]+)|] (\x -> convertNString x)
