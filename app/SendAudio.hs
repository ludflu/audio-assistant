{-# LANGUAGE OverloadedStrings #-}

module SendAudio (sendAudio) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

sendAudio:: FilePath -> IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
sendAudio fp = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyFile fp)
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  liftIO $ print (responseBody r :: Value)

