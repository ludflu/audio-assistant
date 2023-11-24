{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OllamaApi (answerQuestion) where

import Conduit (ConduitM, ConduitT, MonadResource, awaitForever, concatC, concatMapAccumC, concatMapC, concatMapCE, filterC, leftover, mapAccumWhileC, mapC, mapCE, mapM_C, runConduit, runConduitRes, sinkLazy, sourceLazy, yield, (.|))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTQueue, writeTVar)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.RWS as BLS
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, encode, fromJSON, json, parseJSON)
import Data.Aeson.Encoding (string)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BLS
import Data.Conduit.Binary (sinkFile, sinkHandle, sinkLbs, sourceLbs)
import Data.Conduit.Combinators (concatMapE, concatMapM, mapAccumWhile, mapE)
import Data.List (isInfixOf)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
import Data.Text.Array (run)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
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

-- data OllamaResponse = OllamaResponse
--   { model :: String,
--     created_at :: String,
--     response :: String,
--     done :: Bool,
--     context :: Maybe [Int],
--     total_duration :: Maybe Int,
--     load_duration :: Maybe Int,
--     prompt_eval_count :: Int,
--     prompt_eval_duration :: Int,
--     eval_count :: Int,
--     eval_duration :: Int
--   }
--   deriving (Generic, Show)

data OllamaResponse = OllamaResponse
  { model :: String,
    created_at :: String,
    response :: String,
    done :: Bool
  }
  deriving (Generic, Show)

instance ToJSON OllamaRequest

instance FromJSON OllamaResponse

getAnswer :: OllamaResponse -> String
getAnswer = response

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
  let ps :: String = "!.?,"
   in c `elem` ps

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromEnum

stringToByteString :: String -> B.ByteString
stringToByteString = TE.encodeUtf8 . T.pack

byteStringToString :: B.ByteString -> String
byteStringToString = unpack

sentenceChunks :: Monad m => ConduitM String String m ()
sentenceChunks = do
  awaitForever $ \bs -> do
    let (prefix, suffix) = break isPunct bs
    unless (null prefix) $ yield prefix
    unless (null suffix) $ leftover suffix

makeResponseChunk :: B.ByteString -> Maybe OllamaResponse
makeResponseChunk = decode . BLS.fromStrict

writeToMailBox' :: MonadResource m => TQueue String -> String -> m ()
writeToMailBox' mbox msg =
  liftResourceT $
    liftIO $ do
      print "answer:\n"
      print msg
      print "\n"
      atomically $ writeTQueue mbox msg

answerQuestion :: TQueue String -> String -> IO ()
answerQuestion mailbox question = do
  print "sending to api:\n"
  print question
  print "\n"
  forkIO $ answerQuestion' mailbox question
  return ()

concatBytes :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
concatBytes acc chunk = (acc <> chunk, B.empty)

concatString :: String -> String -> (String, String)
concatString acc chunk = (acc <> chunk, [])

stringCombine :: [String] -> String
stringCombine = foldr (\x acc -> x <> acc) []

stringCombine' :: [String] -> [String] -> String
stringCombine' a b = stringCombine $ a <> b

stringContains :: String -> String -> Bool
stringContains a b = b `isInfixOf` a

answerQuestion' :: TQueue String -> String -> IO ()
answerQuestion' mailbox question =
  let payload = OllamaRequest {model = "llama2", prompt = question, stream = False}
      url = "http://192.168.1.200/api/generate"
      apiPort = 11434
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request'' = request' {method = "POST", requestBody = body, port = apiPort}
            request = setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000)) request''
        manager <- newManager tlsManagerSettings
        runResourceT $ do
          rsp <- http request manager
          rlbs <- runConduit $ responseBody rsp .| sinkLbs
          let llamaRsp = fromJust $ parseAnswer rlbs
          mapM_ (writeToMailBox' mailbox) [llamaRsp]

answerQuestion'' :: TQueue String -> String -> IO ()
answerQuestion'' mailbox question =
  let payload = OllamaRequest {model = "llama2", prompt = question, stream = False}
      url = "http://192.168.1.200/api/generate"
      apiPort = 11434
      body = RequestBodyLBS $ encode payload
   in do
        request' <- parseRequest url
        let request'' = request' {method = "POST", requestBody = body, port = apiPort}
            request = setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000)) request''
        manager <- newManager tlsManagerSettings
        runResourceT $
          do
            rsp <- http request manager
            runConduit $
              responseBody rsp
                -- runConduit $
                --   responseBody rsp
                --     .| jsonChunks '}'
                --     .| mapC makeResponseChunk
                --     .| filterC isJust
                --     .| mapC (getAnswer . fromJust)
                --     .| sentenceChunks
                --     .| mapAccumWhile
                --       ( \acc x ->
                --           if stringContains "." acc || stringContains "," acc
                --             then Left x
                --             else Right ((), acc)
                --       )
                --       ()
                --                .| mapC (:)
                -- .| concatMapC (:)
                .| mapM_C (writeToMailBox' mailbox)
