{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SpeechApi (sayText) where

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
    (/:)
  )

newtype SpeechRequest = SpeechRequest
  { message :: String
  }
  deriving (Generic)

newtype SpeechResponse = SpeechResponse
  { duration :: Double
  }
  deriving (Generic)

instance ToJSON SpeechRequest

instance FromJSON SpeechResponse

getDuration :: JsonResponse SpeechResponse -> Double
getDuration jr =
  let sr = responseBody jr
   in duration sr

sayText :: String -> IO Double
sayText message = runReq defaultHttpConfig $ do
  let payload = SpeechRequest message
  let reqBody = ReqBodyJson payload
  let url = "127.0.0.1"

  r <-
    req
      POST
      (http url /: "talk")
      reqBody
      jsonResponse
      $ port 5001
  return $
    getDuration r
