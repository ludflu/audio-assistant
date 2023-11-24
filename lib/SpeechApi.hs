{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpeechApi (sayText) where

import Conduit (ConduitM, ConduitT, MonadResource, awaitForever, concatC, concatMapAccumC, concatMapC, concatMapCE, filterC, leftover, mapAccumWhileC, mapC, mapCE, mapM_C, runConduit, runConduitRes, sinkLazy, sourceLazy, yield, (.|))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTQueue, writeTVar)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, encode, fromJSON, json, parseJSON)
import Data.Aeson.Encoding (string)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BLS
import Data.Char (isPunctuation)
import Data.Conduit.Binary (sinkFile, sinkHandle, sinkLbs, sourceLbs)
import Data.Conduit.Combinators (concatMapE, concatMapM, mapAccumWhile, mapE, splitOnUnboundedE)
import Data.List (isInfixOf)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
import Data.Text.Array (run)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import GHC.Generics (Generic)
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
import Network.HTTP.Req (req)
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLBS, setRequestBodyJSON, setRequestHeaders, setRequestMethod, setRequestPort, setRequestResponseTimeout)
import Network.HTTP.Types

newtype SpeechRequest = SpeechRequest
  { message :: String
  }
  deriving (Generic, Show)

data SpeechResponse = SpeechResponse
  { duration :: Double,
    status :: String
  }
  deriving (Generic, Show)

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
   in do
        request' <- parseRequest url
        let request =
              setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000))
                . setRequestHeaders [(hContentType, "application/json")]
                . setRequestBodyJSON payload
                . setRequestMethod "POST"
                . setRequestPort apiPort
                $ request'

        rsp <- httpLBS request
        liftIO $ print $ getResponseBody rsp
        return $
          fromJust $
            parseDuration $
              getResponseBody rsp
