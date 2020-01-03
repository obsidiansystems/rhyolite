-- | Getting and setting cookies on the frontend.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Rhyolite.Frontend.Cookie where

import Control.Monad ((<=<))
import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM.Document as DOM
import Reflex.Dom.Core
import Web.Cookie

-- | Set or clear the given cookie permanently
--
-- Example:
-- > setPermanentCookie doc =<< defaultCookie "key" (Just "value")
setPermanentCookie :: (MonadJSM m, HasJSContext m) => DOM.Document -> SetCookie -> m ()
setPermanentCookie doc cookie = do
  DOM.setCookie doc $ decodeUtf8 $ LBS.toStrict $ toLazyByteString $ renderSetCookie cookie

-- | Make a cookie with sensible defaults
defaultCookie
  :: (MonadJSM m, HasJSContext m)  -- TODO: verify
  => Text  -- ^ Cookie key
  -> Maybe Text  -- ^ Cookie value (Nothing clears it)
  -> m SetCookie
defaultCookie key mv = do
  currentProtocol <- Reflex.Dom.Core.getLocationProtocol
  pure $ case mv of
    Nothing -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = ""
      , setCookieExpires = Just $ posixSecondsToUTCTime 0
      }
    Just val -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = encodeUtf8 val
      -- We don't want these to expire, but browsers don't support
      -- non-expiring cookies.  Some systems have trouble representing dates
      -- past 2038, so use 2037.
      , setCookieExpires = Just $ UTCTime (fromGregorian 2037 1 1) 0
      , setCookieSecure = currentProtocol == "https:"
      -- This helps prevent CSRF attacks; we don't want strict, because it
      -- would prevent links to the page from working; lax is secure enough,
      -- because we don't take dangerous actions simply by executing a GET
      -- request.
      , setCookieSameSite = if currentProtocol == "file:"
          then Nothing
          else Just sameSiteLax
      }

defaultCookieJson :: (MonadJSM m, HasJSContext m, ToJSON v) => Text -> Maybe v -> m SetCookie
defaultCookieJson k = defaultCookie k . fmap (decodeUtf8 . LBS.toStrict . encode)

setPermanentCookieWithLocation :: (MonadJSM m, HasJSContext m) => DOM.Document -> Maybe ByteString -> Text -> Maybe Text -> m ()
setPermanentCookieWithLocation doc loc key mv = do
  cookie <- defaultCookie key mv
  setPermanentCookie doc $ cookie { setCookieDomain = loc }

-- | Retrieve the current auth token from the given cookie
getCookie :: MonadJSM m => DOM.Document -> Text -> m (Maybe Text)
getCookie doc key = do
  cookieString <- DOM.getCookie doc
  return $ lookup key $ parseCookiesText $ encodeUtf8 cookieString

setPermanentCookieJson :: (MonadJSM m, HasJSContext m, ToJSON v) => DOM.Document -> Text -> Maybe v -> m ()
setPermanentCookieJson d k = setPermanentCookie d <=< defaultCookieJson k

getCookieJson :: (FromJSON v, MonadJSM m) => DOM.Document -> Text -> m (Maybe (Either String v))
getCookieJson d k =
  fmap (eitherDecode . LBS.fromStrict . encodeUtf8) <$> getCookie d k

withPermanentCookieJson :: (MonadJSM m, MonadJSM (Performable m), HasJSContext (Performable m), PerformEvent t m, ToJSON v, FromJSON v)
                        => DOM.Document
                        -> Text
                        -> (Maybe (Either String v) -> m (Event t (Maybe v)))
                        -> m ()
withPermanentCookieJson d k a = do
  cookie0 <- getCookieJson d k
  cookieE <- a cookie0
  performEvent_ $ setPermanentCookieJson d k <$> cookieE
  return ()
