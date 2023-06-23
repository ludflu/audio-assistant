{-# LANGUAGE OverloadedStrings #-}

module SendEmail where

import Control.Monad (when)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as L
import Network.HaskellNet.Auth (AuthType (LOGIN))
import Network.HaskellNet.SMTP.SSL
  ( AuthType (LOGIN),
    authenticate,
    doSMTPSSL,
    sendMail,
  )
import Network.Mail.Mime
  ( Address (Address),
    Mail,
    renderMail',
    simpleMail',
  )

sendGmail :: Mail -> String -> String -> IO ()
sendGmail msg username password = do
  rendered <- renderMail' msg
  doSMTPSSL "smtp.gmail.com" $ \connection -> do
    succeeded <-
      authenticate
        LOGIN
        username
        password
        connection
    when succeeded $
      sendMail
        msg
        connection

email :: Text -> Text -> String -> String -> IO ()
email msgTo msgBody username password = do
  let mail = simpleMail' to from subject (L.fromStrict body)
  sendGmail mail username password
  where
    to = Address Nothing msgTo
    from = Address Nothing (pack username)
    subject = "Hello!"
    body = msgBody
