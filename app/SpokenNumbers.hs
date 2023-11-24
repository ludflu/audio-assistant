module SpokenNumbers (convertNumber) where

import Data.Text
import Text.Numerals

-- convertNumber :: Integer -> Text
-- convertNumber n = toCardinal english n

convertNumber :: Integer -> String
convertNumber n = unpack $ toCardinal english n
