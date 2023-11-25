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

-- {'text': " Ok, genius, who's the fairest of them all?",
-- 'segments': [{'id': 0, 'seek': 0, 'start': 0.0, 'end': 3.0, 'text': " Ok, genius, who's the fairest of them all?", 'tokens': [50364, 3477, 11, 14017, 11, 567, 311, 264, 4865, 372, 295, 552, 439, 30, 50514],
-- 'temperature': 0.0, 'avg_logprob': -0.499399870634079, 'compression_ratio': 0.8936170212765957, 'no_speech_prob': 0.03356686607003212}], 'language': 'en'}

-- getTranscript :: Value -> Text
-- getTranscript (Object response) = case AKM.lookup "text" response of
--   Just (String transcript) -> transcript
--   _ -> "--------------"

getTranscript :: BLS.ByteString -> Either String TranscriptionResponse
getTranscript = eitherDecode

-- TODO can we replace req with http-conduit?
-- sendAudio :: FilePath -> IO String
-- sendAudio fp = runReq defaultHttpConfig $ do
--   let part = LM.partFileSource "file" fp
--   body <- reqBodyMultipart [part]
--   r <-
--     req
--       POST -- method
--       (http "127.0.0.1") -- safe by construction URL
--       body
--       jsonResponse -- specify how to interpret response
--       $ port 5000
--   let response = responseBody r :: Value
--   return $ unpack $ getTranscript response

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
