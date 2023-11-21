{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module OllamaApi (answerQuestion) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), fromJSON, parseJSON)
import qualified Data.Aeson.KeyMap as AKM
import Data.Conduit (ConduitM, runConduitRes, (=$=))
import qualified Data.Conduit.Binary as CB
import Data.Scientific (toRealFloat)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
  ( JsonResponse,
    MonadHttp,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    handleHttpException,
    http,
    jsonResponse,
    port,
    req,
    req',
    reqBodyMultipart,
    responseBody,
    runReq,
    (/:),
  )
import Network.HTTP.Req.Conduit

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

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
    context :: [Int],
    total_duration :: Int,
    load_duration :: Int,
    prompt_eval_count :: Int,
    prompt_eval_duration :: Int,
    eval_count :: Int,
    eval_duration :: Int
  }
  deriving (Generic)

instance ToJSON OllamaRequest

instance FromJSON OllamaResponse

getAnswer :: JsonResponse OllamaResponse -> String
getAnswer jr =
  let sr = responseBody jr
   in response sr

answerQuestion :: String -> IO String
answerQuestion question = runReq defaultHttpConfig $ do
  let payload = OllamaRequest {model = "llama2", prompt = "In one sentence: " ++ question, stream = False}
  let reqBody = ReqBodyJson payload
  let url = "127.0.0.1"
  let apiPort = 11434

  r <-
    req
      POST
      (http url /: "api" /: "generate")
      reqBody
      jsonResponse
      $ port apiPort
  return $
    getAnswer r

-- answerQuestion2 :: String -> IO String
answerQuestion2 question = runConduitRes $ do
  let payload = OllamaRequest {model = "llama2", prompt = "In one sentence: " ++ question, stream = False}
  let reqBody = ReqBodyJson payload
  let url = "127.0.0.1"
  let apiPort = 11434
  req' POST (http url /: "api" /: "generate") reqBody mempty httpSource
    =$= CB.sinkFile "/tmp/my-favorite-file.bin"