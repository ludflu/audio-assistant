{-# LANGUAGE OverloadedStrings #-}
import Network.Mail.Mime
import Network.Mail.SMTP

main :: String -> String IO ()
main msgTo msgBody = do
  let mail = simpleMail' to from subject body
  renderSendMail (SMTPSTARTTLS "smtp.gmail.com" defaultSettingsSMTPSTARTTLS { smtpUsername = Just "your-email@gmail.com"
                                                                           , smtpPassword = Just "your-password"
                                                                           }) mail

  where
    to = Address Nothing msgTo
    from = Address Nothing "sender@example.com"
    subject = "Hello!"
    body = plainPart msgBody


