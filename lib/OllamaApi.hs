{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module OllamaApi (askQuestion) where

import Control.Monad.IO.Class ()
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), fromJSON, parseJSON)
import qualified Data.Aeson.KeyMap as AKM
import Data.Scientific (toRealFloat)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
  ( JsonResponse,
    POST (POST),
    ReqBodyJson (ReqBodyJson),
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    reqBodyMultipart,
    responseBody,
    runReq,
    (/:),
  )

newtype OllamaRequest = OllamaRequest
  { model :: String,
    prompt :: String,
    stream :: Bool
  }
  deriving (Generic)

newtype OllamaResponse = OllamaResponse
  { model :: String,
    created_at :: String,
    response :: String,
    done :: Bool,
    total_duration :: Int,
    load_duration :: Int,
    sample_count :: Int,
    sample_duration :: Int,
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
   in answer sr

askQuestion :: String -> IO String
askQuestion question = runReq defaultHttpConfig $ do
  let payload = OllamaRequest ("In one sentence: " + question)
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
