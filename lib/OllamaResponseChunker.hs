{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaResponseChunker (chunker) where

import Conduit
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), fromJSON, parseJSON)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Conduit.Binary as CB
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)

data OllamaResponse = OllamaResponse
  { model :: String,
    created_at :: String,
    response :: String
  }
  deriving (Generic)

instance FromJSON OllamaResponse

packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

testString :: BSL.ByteString
testString = BSL.fromStrict $ packStr "{\"test\":\"1 bla\" \"done\"=False}{\"test\":\"2 bla\" \"done\"=False}{\"test\":\"3 bla\" \"done\"=False}"

conduitChunks :: Monad m => Char -> ConduitT BSL.ByteString BSL.ByteString m ()
conduitChunks trigger = do
  let triggerByte = fromIntegral (fromEnum trigger)
  awaitForever $ \bs -> do
    let (prefix, suffix) = BSL.break (== triggerByte) bs
    unless (BSL.null prefix) $ yield $ prefix <> "}"
    unless (BSL.null suffix) $ do
      let rest = BSL.drop 1 suffix
      unless (BSL.null rest) $ leftover rest

chunker :: IO ()
chunker =
  runConduitRes $
    yield testString
      .| conduitChunks '}'
      .| mapM_C (liftIO . putStrLn . ("Processing chunk: " ++) . show)