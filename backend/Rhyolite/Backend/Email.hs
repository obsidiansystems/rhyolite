-- | Utilities and templates to send emails from the backend.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.Email where

import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
import GHC.Generics (Generic)
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL hiding (sendMail)
import Network.Mail.Mime (Address (..), Mail (..), htmlPart)
import Network.Mail.SMTP (simpleMail)
import Network.Socket (HostName, PortNumber)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

import Rhyolite.Backend.Schema.TH (deriveNewtypePersistBackend)
import Rhyolite.Email
import Rhyolite.Route
import Rhyolite.Sign
import Rhyolite.TH (embedFile)

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

instance FromJSON PortNumber where
  parseJSON v = do
    n :: Word16 <- parseJSON v
    return $ fromIntegral n

instance ToJSON PortNumber where
  toJSON n = toJSON (fromIntegral n :: Word16)

newtype EmailT m a = EmailT { unEmailT :: ReaderT EmailEnv m a } deriving (Functor, Applicative, Monad, MonadIO, MonadRoute r, MonadSign, MonadTrans, MonadLogger)

instance MonadIO m => MonadEmail (EmailT m) where
  sendMail mail = do
    env <- EmailT ask
    liftIO $ putStrLn $ "Sending email " <> show (map snd $ filter ((=="Subject") . fst) $ mailHeaders mail) <> " to " <> show (map addressEmail $ mailTo mail)
    liftIO $ sendEmail env mail

runEmailT :: EmailT m a -> EmailEnv -> m a
runEmailT = runReaderT . unEmailT

sendEmailFrom :: MonadEmail m
              => Text -- ^ Sender name
              -> Text -- ^ Sender email
              -> NonEmpty Text -- ^ Recipients
              -> Text -- ^ Subject line
              -> Html -- ^ Body of message
              -> m ()
sendEmailFrom name' email recipients sub body =
  sendMail $ simpleMail (Address (Just name') email)
                        (map (Address Nothing) $ toList recipients)
                        []
                        []
                        sub
                        [htmlPart $ renderHtml body]

deriveNewtypePersistBackend (\m -> [t| EmailT $m |]) (\m -> [t| ReaderT EmailEnv $m |]) 'EmailT 'unEmailT

emailTemplate :: (MonadRoute r m, Default r) => Text -> Maybe Html -> Html -> Html -> Html -> m Html
emailTemplate productName mStyleHtml titleHtml leadHtml contentHtml =
  emailTemplateSimple productName mStyleHtml titleHtml $ H.table $ do
    H.tr $ H.td $ H.h1 titleHtml
    H.hr
    H.tr $ H.td $ H.p ! class_ "lead" $ leadHtml
    H.hr
    H.tr $ H.td $ contentHtml

emailTemplateSimple :: (MonadRoute r m, Default r) => Text -> Maybe Html -> Html -> Html -> m Html
emailTemplateSimple productName mStyleHtml titleHtml contentHtml = do
  indexLink <- routeToUrl def
  return $ H.docTypeHtml $ do
    H.head $ do
      H.style $ case mStyleHtml of
        Nothing -> H.toHtml $ decodeUtf8 $(embedFile "email.css")
        Just styleHtml -> styleHtml
      H.title titleHtml
    H.body $ H.table $ do
      H.tr $ H.td $ contentHtml
      H.tr $ H.td $ H.table $ H.tr $ H.td $ do
        H.hr
        H.p $ do
          H.text "Brought to you by "
          H.a ! A.href (fromString $ show indexLink) $ H.toHtml productName
