{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaApi (answerQuestion) where

-- import Network.HTTP.Client (BodyReader, Response)
-- import qualified Network.HTTP.Client.MultipartFormData as LM

import Conduit (runConduit, (.|))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), encode, fromJSON, parseJSON)
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString (ByteString, toStrict)
import Data.Conduit.Binary (sinkFile, sinkHandle, sinkLbs)
import Data.Conduit.List
import GHC.Generics (Generic)
import Network.HTTP.Conduit
  ( Request (method, port, requestBody, requestHeaders, secure),
    RequestBody (RequestBodyBS, RequestBodyLBS),
    Response (responseBody),
    http,
    httpLbs,
    newManager,
    parseRequest,
    tlsManagerSettings,
  )
import Network.HTTP.Types
import OllamaResponseChunker (chunker, chunker2)

data OllamaRequest = OllamaRequest
  { model :: String,
    prompt :: String,
    stream :: Bool
  }
  deriving (Generic)

data OllamaResponse = OllamaResponse
  { model :: String,
    created_at :: String,
    response :: String,
    done :: Bool,
    context :: Maybe [Int],
    total_duration :: Maybe Int,
    load_duration :: Maybe Int,
    prompt_eval_count :: Int,
    prompt_eval_duration :: Int,
    eval_count :: Int,
    eval_duration :: Int
  }
  deriving (Generic)

instance ToJSON OllamaRequest

instance FromJSON OllamaResponse

answerQuestion :: String -> IO ByteString
answerQuestion question =
  let payload = OllamaRequest {model = "llama2", prompt = question, stream = False}
      url = "http://127.0.0.1/api/generate"
      apiPort = 11434
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request = request' {method = "POST", requestBody = body, port = apiPort}
        manager <- newManager tlsManagerSettings
        runResourceT $ do
          response <- http request manager
          rlbs <- runConduit $ responseBody response .| sinkLbs
          return $ toStrict rlbs
