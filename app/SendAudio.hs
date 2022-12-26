{-# LANGUAGE OverloadedStrings #-}

module SendAudio (sendAudio) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req
import qualified Network.HTTP.Client.MultipartFormData as LM

sendAudio:: FilePath -> IO ()
sendAudio fp = runReq defaultHttpConfig $ do
  let part = LM.partFileSource "file" fp 
  body <- reqBodyMultipart [part]
  r <- req
      POST -- method
      (http "127.0.0.1") -- safe by construction URL
      (body)
      jsonResponse -- specify how to interpret response
      $ port 5000
  liftIO $ print (responseBody r :: Value)
