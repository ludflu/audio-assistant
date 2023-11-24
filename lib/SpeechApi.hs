{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpeechApi (sayText) where

import Conduit (runConduit, runResourceT, (.|))
import Control.Monad.IO.Class ()
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, encode, fromJSON, parseJSON)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BLS
import Data.Conduit.Binary (sinkLbs)
import Data.Maybe (fromJust)
import Data.Scientific (toRealFloat)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Conduit
  ( Request (method, port, requestBody, requestHeaders, responseTimeout, secure),
    RequestBody (RequestBodyBS, RequestBodyLBS),
    Response (responseBody),
    http,
    httpLbs,
    newManager,
    parseRequest,
    responseTimeoutMicro,
    tlsManagerSettings,
  )
import Network.HTTP.Simple (setRequestResponseTimeout)

newtype SpeechRequest = SpeechRequest
  { message :: String
  }
  deriving (Generic)

data SpeechResponse = SpeechResponse
  { duration :: Double,
    status :: String
  }
  deriving (Generic)

instance ToJSON SpeechRequest

instance FromJSON SpeechResponse

parseDuration :: BLS.ByteString -> Maybe Double
parseDuration rsp =
  let srsp = decode rsp
   in fmap duration srsp

sayText :: String -> IO Double
sayText msg =
  let payload = SpeechRequest {message = msg}
      url = "http://127.0.0.1/talk"
      apiPort = 5002
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request'' = request' {method = "POST", requestBody = body, port = apiPort}
            request = setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000)) request''
        manager <- newManager tlsManagerSettings
        runResourceT $ do
          rsp <- http request manager
          rlbs <- runConduit $ responseBody rsp .| sinkLbs
          return $ fromJust $ parseDuration rlbs
