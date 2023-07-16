{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DavinciApi (askQuestion) where

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

newtype DavinciRequest = DavinciRequest
  { question :: String
  }
  deriving (Generic)

newtype DavinciResponse = DavinciResponse
  { answer :: String
  }
  deriving (Generic)

instance ToJSON DavinciRequest

instance FromJSON DavinciResponse

getAnswer :: JsonResponse DavinciResponse -> String
getAnswer jr =
  let sr = responseBody jr
   in answer sr

askQuestion :: String -> IO String
askQuestion question = runReq defaultHttpConfig $ do
  let payload = DavinciRequest question
  let reqBody = ReqBodyJson payload
  let url = "127.0.0.1"

  r <-
    req
      POST
      (http url /: "ask")
      reqBody
      jsonResponse
      $ port 5005
  return $
    getAnswer r
