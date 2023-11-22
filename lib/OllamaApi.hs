{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaApi (answerQuestion) where

import Conduit (ConduitM, ConduitT, awaitForever, filterC, leftover, mapC, mapM_C, runConduit, runConduitRes, sinkLazy, sourceLazy, yield, (.|))
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.RWS as BLS
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, encode, fromJSON, json, parseJSON)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BLS
import Data.Conduit.Binary (sinkFile, sinkHandle, sinkLbs, sourceLbs)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
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
import Network.HTTP.Simple (setRequestResponseTimeout)
import Network.HTTP.Types

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
  deriving (Generic, Show)

instance ToJSON OllamaRequest

instance FromJSON OllamaResponse

getAnswer :: OllamaResponse -> String
getAnswer = response

jsonChunks :: Monad m => Char -> ConduitM B.ByteString B.ByteString m ()
jsonChunks trigger = do
  let triggerByte = fromIntegral (fromEnum trigger)
  awaitForever $ \bs -> do
    let (prefix, suffix) = B.break (== triggerByte) bs
    unless (B.null prefix) $ yield $ prefix <> "}"
    unless (B.null suffix) $ do
      let rest = B.drop 1 suffix
      unless (B.null rest) $ leftover rest

isPunct :: Char -> Bool
isPunct c =
  let ps :: String = "!.?'"
   in c `elem` ps

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromEnum

stringToByteString :: String -> B.ByteString
stringToByteString = TE.encodeUtf8 . T.pack

sentenceChunks :: Monad m => ConduitM B.ByteString B.ByteString m ()
sentenceChunks = do
  awaitForever $ \bs -> do
    let (prefix, suffix) = B.break (isPunct . word8ToChar) bs
    unless (B.null prefix) $ yield prefix
    unless (B.null suffix) $ leftover suffix

makeResponseChunk :: B.ByteString -> Maybe OllamaResponse
makeResponseChunk = decode . BLS.fromStrict

-- answerQuestion :: String -> IO ()
answerQuestion question =
  let payload = OllamaRequest {model = "llama2", prompt = question, stream = False}
      url = "http://127.0.0.1/api/generate"
      apiPort = 11434
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request'' = request' {method = "POST", requestBody = body, port = apiPort}
            request = setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000)) request''
        manager <- newManager tlsManagerSettings
        runResourceT $ do
          response <- http request manager
          runConduit $
            responseBody response
              .| jsonChunks '}'
              .| mapC makeResponseChunk
              .| filterC isJust
              .| mapC fromJust
              .| mapC getAnswer
              .| mapC stringToByteString
              .| sentenceChunks
              .| mapM_C (liftIO . putStrLn . ("Processing chunk: " ++) . show)
