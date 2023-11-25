{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaResponseChunker (chunker) where

import Conduit
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, fromJSON, parseJSON)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as BLS
import Data.ByteString.UTF8 as BSU
import Data.Char
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Combinators (mapAccumWhile, splitOnUnboundedE)
import Data.List (isInfixOf)
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import GHC.Generics (Generic)
import OllamaApi

data OllamaResponse = OllamaResponse
  { model :: String,
    created_at :: String,
    response :: String
  }
  deriving (Generic, Show)

instance FromJSON OllamaResponse

stringContains :: String -> String -> Bool
stringContains a b = b `isInfixOf` a

chunker :: IO ()
chunker = do
  s <- readFile "test.json"
  print "Starting"
  runConduitRes $
    yield (BSU.fromString s)
      .| jsonChunks '}'
      .| mapC makeResponseChunk
      .| filterC isJust
      .| mapC (getAnswer . fromJust)
      .| splitOnUnboundedE isPunctuation
      .| mapM_C (liftIO . print . ("Processing chunk: " ++) . show)
  print "done"
