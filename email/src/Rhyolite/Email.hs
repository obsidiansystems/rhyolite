{-| Description: Build and send emails

Utilities and templates to send emails from the backend.
-}

{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language PolyKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Email where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Aeson
import Data.Default
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybeToList)
import Data.Signed
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC2822
import Data.Word
import GHC.Generics (Generic)
import Network.HaskellNet.Auth
import Network.HaskellNet.SMTP.SSL hiding (sendMail)
import qualified Network.HaskellNet.SMTP.SSL as HaskellNet
import Network.Mail.Mime (Mail)
import Network.Mail.Mime (Address(..), Mail(..), htmlPart, plainPart)
import Network.Mail.SMTP (simpleMail)
import Network.Socket (HostName, PortNumber)
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Builder.Static
import Rhyolite.Route
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 ((!), Html)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes as A

-- | Errors that can arise while interacting with an SMTP server
data EmailError
   = EmailError_Connection Text
   -- ^ An error occurred while trying to connect to the SMTP server
   | EmailError_Exception SMTPException
   -- ^ An error occurred while interacting with an SMTP server
  deriving (Generic, Show)

instance Exception EmailError

-- | A monad that can send emails
class Monad m => MonadEmail m where
  sendMail :: Mail -> m (Either EmailError ())

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail

instance MonadEmail m => MonadEmail (MaybeT m) where
  sendMail = lift . sendMail

instance MonadEmail m => MonadEmail (ExceptT e m) where
  sendMail = lift . sendMail

-- TODO upstream these orphan instances
deriving instance Generic AuthType
instance ToJSON AuthType
instance FromJSON AuthType
instance FromJSON PortNumber where
  parseJSON v = do
    n :: Word16 <- parseJSON v
    return $ fromIntegral n
instance ToJSON PortNumber where
  toJSON n = toJSON (fromIntegral n :: Word16)

-- | SMTP connection protocols
data SMTPProtocol
  = SMTPProtocol_Plain
  | SMTPProtocol_SSL
  | SMTPProtocol_STARTTLS
  deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON SMTPProtocol
instance ToJSON SMTPProtocol

-- | Configuration for how to authenticate with a mail server
data EmailAuth = EmailAuth
  { _emailAuth_authType :: AuthType
  , _emailAuth_username :: Text
  , _emailAuth_password :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EmailAuth
instance FromJSON EmailAuth

-- | Configuration for how to find an email server, and how to
-- authenticate with it.
data EmailConfig = EmailConfig
  { _emailConfig_hostname :: HostName -- ^ E.g., "smtp.server.com"
  , _emailConfig_port :: Word16
  , _emailConfig_protocol :: SMTPProtocol
  , _emailConfig_emailAuth :: Maybe EmailAuth
  } deriving (Show, Eq, Generic)

instance ToJSON EmailConfig
instance FromJSON EmailConfig

-- | Send an email using the provided connection info. This function ignores
-- send errors.
sendEmail :: EmailConfig -> Mail -> IO (Either EmailError ())
sendEmail ee m = withSMTP ee $ HaskellNet.sendMail m

-- | Run an IO action that expects an active connection to an smtp server.
-- The action returns an error if that connection cannot be established.
withSMTP :: EmailConfig -> (SMTPConnection -> IO a) -> IO (Either EmailError a)
withSMTP cfg send = do
  let hostname = _emailConfig_hostname cfg
      port = fromIntegral $ _emailConfig_port cfg
      go conn = case _emailConfig_emailAuth cfg of
        Nothing -> Right <$> send conn
        Just (EmailAuth authType un pw) -> do
          loginResult <- authenticate authType (T.unpack un) (T.unpack pw) conn
          if loginResult
            then Right <$> send conn
            else pure $ Left (EmailError_Connection "withSMTP: authenticate command failed")
  er <- try $ case _emailConfig_protocol cfg of
    SMTPProtocol_Plain -> doSMTPPort hostname port go
    SMTPProtocol_STARTTLS -> doSMTPSTARTTLSWithSettings hostname (defaultSettingsSMTPSTARTTLS { sslPort = port }) go
    SMTPProtocol_SSL -> doSMTPSSLWithSettings hostname (defaultSettingsSMTPSSL { sslPort = port }) go
  case er of
    Left err -> pure $ Left (EmailError_Exception err)
    Right r -> pure $ r

-- | A monad transformer that can send emails
newtype EmailT m a = EmailT { unEmailT :: ReaderT EmailConfig m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadRoute r
    , MonadSign
    , MonadTrans
    , MonadLogger
    )

instance MonadIO m => MonadEmail (EmailT m) where
  sendMail mail = do
    env <- EmailT ask
    liftIO $ putStrLn $ mconcat
      [ "Sending email "
      , show (map snd $ filter ((=="Subject") . fst) $ mailHeaders mail)
      , " to "
      , show (map addressEmail $ mailTo mail)
      ]
    liftIO $ sendEmail env mail

-- | Run an 'EmailT' action
runEmailT :: EmailT m a -> EmailConfig -> m a
runEmailT = runReaderT . unEmailT

-- | Send an email, specifiying the "from" fields
sendEmailFrom :: MonadEmail m
              => Text -- ^ Sender name
              -> Text -- ^ Sender email
              -> NonEmpty Text -- ^ Recipients
              -> Text -- ^ Subject line
              -> Html -- ^ Body of message
              -> m (Either EmailError ())
sendEmailFrom name' email recipients sub body =
  sendMail $ simpleMail (Address (Just name') email)
                        (map (Address Nothing) $ toList recipients)
                        []
                        []
                        sub
                        [htmlPart $ renderHtml body]

-- | Configuration for app-specific emails
data WidgetEmailCfg br fr = WidgetEmailCfg
  { _widgetEmailName :: Text
  -- ^ Name to use in the "from:" field.
  , _widgetEmailAddress :: Text
  -- ^ Email address to use in the "from:" field.
  , _widgetEmailBaseURL :: Text
  -- ^ Base URL to build URLs for
  , _widgetEmailRouteEncoder :: (Encoder Identity Identity (R (FullRoute br fr)) PageName)
  -- ^ Encoder to use for routes; usually the project's checkedRouteEncoder
  }

-- | Build and send an email using a StaticWidget that can use frontend routes.
sendWidgetEmailFrom
  :: forall k (x :: k) a t r br m.
     (MonadIO m, MonadEmail m, Reflex t)
  => WidgetEmailCfg br r
  -- ^ Configuration for email sender
  -> NonEmpty Text
  -- ^ List of recipients
  -> Text
  -- ^ Subject line
  -> Maybe ((RouteToUrlT (R r) Identity) Text)
  -- ^ Body plaintext, with route decoder
  -> SetRouteT t (R r) (RouteToUrlT (R r) (StaticWidget x)) a
  -- ^ Body widget for the email
  -> m (Either EmailError ())
sendWidgetEmailFrom cfg recipients sub plainText bodyWidget =
  sendMail =<< widgetMail cfg recipients sub plainText bodyWidget

-- | Build an email using a StaticWidget that can use frontend routes.
widgetMail
  :: forall k (x :: k) a t r br m.
     (MonadIO m, MonadEmail m, Reflex t)
  => WidgetEmailCfg br r
  -- ^ Configuration for email sender
  -> NonEmpty Text
  -- ^ List of recipients
  -> Text
  -- ^ Subject line
  -> Maybe ((RouteToUrlT (R r) Identity) Text)
  -- ^ Body plaintext, with route decoder
  -> SetRouteT t (R r) (RouteToUrlT (R r) (StaticWidget x)) a
  -- ^ Body widget for the email
  -> m Mail
widgetMail cfg recipients sub plainText bodyWidget = do
  let
    WidgetEmailCfg
        { _widgetEmailName = name'
        , _widgetEmailAddress = email
        , _widgetEmailBaseURL = baseUrl
        , _widgetEmailRouteEncoder = routeEncoder } = cfg
    renderRouteForEmail = (baseUrl <>) . renderFrontendRoute routeEncoder
    runEmailWidget = fmap snd . renderStatic . flip runRouteToUrlT renderRouteForEmail . runSetRouteT
    runEmailPlaintext = runIdentity . flip runRouteToUrlT renderRouteForEmail
  t <- liftIO $ getCurrentTime
  let formattedTime = formatTimeRFC2822 $ utcToZonedTime utc t
  body <- liftIO $ LT.fromStrict . decodeUtf8 <$> runEmailWidget bodyWidget
  let bodyText = LT.fromStrict . runEmailPlaintext <$> plainText
  -- XXX this can probably all be done applicatively
  return $ Mail
    (Address (Just name') email)
    (map (Address Nothing) $ toList recipients)
    []
    []
    [("Subject", sub), ("Date", formattedTime)]
    [maybeToList (plainPart <$> bodyText) <> [htmlPart body]]

-- | A basic email template
emailTemplate :: (MonadRoute r m, Default r) => Text -> Maybe Html -> Html -> Html -> Html -> m Html
emailTemplate productName mStyleHtml titleHtml leadHtml contentHtml =
  emailTemplateSimple productName mStyleHtml titleHtml $ H.table $ do
    H.tr $ H.td $ H.h1 titleHtml
    H.hr
    H.tr $ H.td $ H.p ! class_ "lead" $ leadHtml
    H.hr
    H.tr $ H.td $ contentHtml

-- | An email template with a predefined layout
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
