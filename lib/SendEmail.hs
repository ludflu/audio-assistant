{-# LANGUAGE OverloadedStrings #-}

module SendEmail where

import ChatLogger (getAnswersForLastQuestion)
import ConfigParser (EnvConfig (mailUser), mailPassword, mailServer)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, ask, liftIO, runReaderT)
import Control.Monad.State (get)
import Data.Maybe (maybeToList)
import Data.Text (Text, intercalate, pack)
import qualified Data.Text.Lazy as L
import Listener (ListenerMonad, ListenerState (dbPool), say)
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

sendGmail :: String -> Mail -> String -> String -> IO ()
sendGmail smtpServer msg username password = do
  rendered <- renderMail' msg
  doSMTPSSL smtpServer $ \connection -> do
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

email :: Text -> Text -> String -> String -> String -> IO ()
email msgTo msgBody server username password = do
  let mail = simpleMail' to from subject (L.fromStrict body)
  sendGmail server mail username password
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
      msg = pack note
  mapM_ (\(user, password) -> liftIO $ email (pack user) msg (mailServer config) user password) userPwd

sendEmailAnswer :: ListenerMonad ()
sendEmailAnswer = do
  st <- get
  config <- ask
  answer <- liftIO $ getAnswersForLastQuestion (dbPool st)
  let answerLines = concat $ maybeToList answer
      answerMsg = intercalate "\n" $ map pack answerLines
      userPwd = getUserPwd config
  mapM_ (\(user, password) -> liftIO $ email (pack user) answerMsg (mailServer config) user password) userPwd
  say "Answer Emailed!"
  return ()
