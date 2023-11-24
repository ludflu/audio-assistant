{-# LANGUAGE QuasiQuotes #-}

module SpokenNumbers (convertNumber, convertNString, convertAllNumbers) where

import qualified Data.Text as T
import Text.Numerals
import Text.Regex.PCRE.Heavy (Regex, gsub, re)

-- convertNumber :: Integer -> Text
-- convertNumber n = toCardinal english n

convertNumber :: Integer -> String
convertNumber n = T.unpack $ toCardinal english n

removeCommas :: String -> String
removeCommas s = filter (\x -> x /= ',') s

convertNString :: String -> String
convertNString nstr =
  let i = read $ removeCommas nstr
   in convertNumber i

convertAllNumbers :: String -> String
convertAllNumbers = gsub [re|([\d,]+)|] (\x -> convertNString x)
