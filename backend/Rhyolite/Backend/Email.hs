{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.Email where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL
import Network.Mail.Mime (Mail (..))
import Network.Socket (PortNumber, HostName)

data SMTPProtocol = SMTPProtocol_Plain
                  | SMTPProtocol_SSL
                  | SMTPProtocol_STARTTLS
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON SMTPProtocol
instance ToJSON SMTPProtocol

type EmailEnv = (HostName, SMTPProtocol, PortNumber, UserName, Password)

sendEmail :: EmailEnv -> Mail -> IO ()
sendEmail ee m = void $ withSMTP ee $ sendMimeMail2 m

withSMTP :: EmailEnv -> (SMTPConnection -> IO a) -> IO (Either Text a)
withSMTP  (hostname, protocol, port, un, pw) a = let
  go c = do
    case un of
      [] -> Right <$> a c
      _ -> do
        loginResult <- authenticate LOGIN un pw c
        if loginResult
          then Right <$> a c
          else return $ Left "Login failed"
  in case protocol of
    SMTPProtocol_Plain -> doSMTPPort hostname port go
    SMTPProtocol_STARTTLS -> doSMTPSTARTTLSWithSettings hostname (defaultSettingsSMTPSTARTTLS { sslPort = port }) go
    SMTPProtocol_SSL -> doSMTPSSLWithSettings hostname (defaultSettingsSMTPSSL { sslPort = port }) go
