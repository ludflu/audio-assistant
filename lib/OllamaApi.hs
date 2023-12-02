{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaApi (answerQuestion, getAnswer, makeResponseChunk, jsonChunks, chunker) where

import ChatLogger
import ChatLoggerSchema
import Conduit (ConduitM, ConduitT, MonadResource, awaitForever, concatC, concatMapAccumC, concatMapC, concatMapCE, filterC, leftover, mapAccumWhileC, mapC, mapCE, mapM_C, runConduit, runConduitRes, sinkLazy, sourceLazy, yield, (.|))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTQueue, writeTVar)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, encode, fromJSON, parseJSON)
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
import Data.Time (getCurrentTime)
import Data.Word (Word8)
import Database.Persist.Postgresql (ConnectionPool)
import GHC.Generics (Generic)
import Listener
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
    done :: Bool
  }
  deriving (Generic, Show)

instance ToJSON OllamaRequest

instance FromJSON OllamaResponse

extractAnswer :: OllamaResponse -> String
extractAnswer = response

parseAnswer :: BLS.ByteString -> Maybe String
parseAnswer llamaRsp =
  let rsp = decode llamaRsp
   in fmap response rsp

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
  let ps :: String = "!.?"
   in c `elem` ps

makeResponseChunk :: B.ByteString -> Maybe OllamaResponse
makeResponseChunk = decode . BLS.fromStrict

writeToMailBox' :: MonadResource m => TQueue String -> Maybe QueryId -> Maybe ConnectionPool -> String -> m ()
writeToMailBox' mbox queryId dbPool msg =
  liftResourceT $
    liftIO $ do
      mapM_ (\qid -> addAnswer dbPool $ Answer qid msg) queryId
      atomically $ writeTQueue mbox msg

getDbStuff :: Maybe QueryId -> Maybe ConnectionPool -> Maybe (QueryId, ConnectionPool)
getDbStuff q d = do
  q' <- q
  d' <- d
  return (q', d')

answerQuestion :: String -> Int -> TQueue String -> String -> Maybe QueryId -> Maybe ConnectionPool -> IO ()
answerQuestion url port mailbox question queryId dbPool = do
  print "sending to api:\n"
  print question
  forkIO $ answerQuestion' url port mailbox queryId dbPool question
  return ()

chunker :: Monad m => (String -> m ()) -> ConduitT B.ByteString c m ()
chunker chunkAction =
  jsonChunks '}'
    .| mapC makeResponseChunk
    .| filterC isJust
    .| mapC (extractAnswer . fromJust)
    .| splitOnUnboundedE isPunct -- TODO can we split on more than one character so we don't split decimal points?
    .| mapM_C chunkAction

answerQuestion' :: String -> Int -> TQueue String -> Maybe QueryId -> Maybe ConnectionPool -> String -> IO ()
answerQuestion' url apiPort mailbox qid dbPool question =
  let payload = OllamaRequest {model = "llama2", prompt = question, stream = True}
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request'' = request' {method = "POST", requestBody = body, port = apiPort}
            request = setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000)) request''
            chunkAction = writeToMailBox' mailbox qid dbPool
        manager <- newManager tlsManagerSettings
        runResourceT $
          do
            rsp <- http request manager
            runConduit $
              responseBody rsp .| chunker chunkAction
