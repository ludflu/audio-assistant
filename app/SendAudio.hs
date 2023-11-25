{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SendAudio (sendAudio) where

import Conduit (runResourceT)
import Control.Monad.IO.Class ()
import Data.Aeson (FromJSON, Value (Object, String), eitherDecode)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BLS
import Data.Conduit.Binary (sinkLbs)
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileSource)
import qualified Network.HTTP.Client.MultipartFormData as LM
import Network.HTTP.Conduit
  ( Request (method, port, requestBody, requestHeaders, responseTimeout, secure),
    RequestBody (RequestBodyBS, RequestBodyLBS),
    Response (responseBody),
    http,
    httpLbs,
    newManager,
    parseRequest,
    parseUrl,
    responseTimeoutMicro,
    tlsManagerSettings,
  )
import Network.HTTP.Simple (getResponseBody, httpLBS, setRequestBody, setRequestBodyFile, setRequestMethod, setRequestPort, setRequestResponseTimeout)

data TranscriptionResponse = TranscriptionResponse
  { text :: String,
    language :: String
  }
  deriving (Generic, Show)

instance FromJSON TranscriptionResponse

getTranscript :: BLS.ByteString -> Either String TranscriptionResponse
getTranscript = eitherDecode

sendAudio :: String -> Int -> FilePath -> IO String
sendAudio url port fp =
  do
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    let filesource = partFileSource "file" fp
        request' =
          setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000))
            . setRequestMethod "POST"
            . setRequestPort port
            $ request
    r <- formDataBody [filesource] request'
    rsp <- httpLbs r manager
    let response = getTranscript $ getResponseBody rsp
     in return $ case response of
          Left err -> "" -- liftIO $ print ("transcription error" ++ err) >> return ""
          Right transcriptionResponse -> text transcriptionResponse
