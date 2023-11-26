{-# LANGUAGE OverloadedStrings #-}

module SendEmail where

import ConfigParser (EnvConfig (mailUser), mailPassword)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, liftIO, runReaderT)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as L
import Listener (ListenerMonad)
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
import RecordNote (getNote, readNote)

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

getUserPwd :: EnvConfig -> Maybe (String, String)
getUserPwd config = do
  user <- mailUser config
  password <- mailPassword config
  return (user, password)

sendEmailNote :: ListenerMonad ()
sendEmailNote = do
  note <- liftIO getNote
  config <- ask
  let userPwd = getUserPwd config
  let msg = pack note
  _ <- mapM_ (\(u, p) -> liftIO $ email (pack u) msg u p) userPwd
  return ()