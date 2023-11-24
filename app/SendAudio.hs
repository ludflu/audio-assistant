{-# LANGUAGE OverloadedStrings #-}

module SendAudio (sendAudio) where

import Control.Monad.IO.Class ()
import Data.Aeson (Value (Object, String))
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text, unpack)
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Req
  ( POST (POST),
    defaultHttpConfig,
    http,
    jsonResponse,
    port,
    req,
    reqBodyMultipart,
    responseBody,
    runReq,
  )

getTranscript :: Value -> Text
getTranscript (Object response) = case AKM.lookup "text" response of
  Just (String transcript) -> transcript
  _ -> "--------------"

-- TODO can we replace req with http-conduit?
sendAudio :: FilePath -> IO String
sendAudio fp = runReq defaultHttpConfig $ do
  let part = LM.partFileSource "file" fp
  body <- reqBodyMultipart [part]
  r <-
    req
      POST -- method
      (http "127.0.0.1") -- safe by construction URL
      body
      jsonResponse -- specify how to interpret response
      $ port 5000
  let response = responseBody r :: Value
  return $ unpack $ getTranscript response
