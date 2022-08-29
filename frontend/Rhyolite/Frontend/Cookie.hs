{-|
Description:
  Getting and setting cookies

Getting and setting cookies on the frontend. Cookies are base64 encoded.

There's some overlap between the functions in this module and
Obelisk.Frontend.Cookie. That module provides 'askCookies', which can also be
used to retrieve cookies. The Obelisk module has the advantage of working
server-side as well, so that widgets that depend on the cookie can be
prerendered. The functions in this module use javascript and, so, cannot be
rendered server-side. If you're mixing the two, bear in mind that obelisk does
not currently demand that cookies be base64-encoded, while this module does,
so you'll have to base64-decode the result of askCookies yourself.
-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
module Rhyolite.Frontend.Cookie where

import Control.Monad ((<=<))
import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified GHCJS.DOM.Document as DOM
import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom.Core
import Web.Cookie

-- | "To maximize compatibility with user agents, servers that wish to
-- store arbitrary data in a cookie-value SHOULD encode that data, for
-- example, using Base64"
-- <https://www.rfc-editor.org/rfc/rfc6265 RFC 6265: HTTP State Management Mechanism>
base64EncodeCookie
  :: SetCookie
  -> SetCookie
base64EncodeCookie c = c { setCookieValue = B64.encode (setCookieValue c) }

-- | A synonym for Data.ByteString.Base64.decode because there are too many
-- functions called "decode".
base64Decode :: ByteString -> Either String ByteString
base64Decode = B64.decode

-- | Set or clear the given cookie permanently
--
-- Example:
-- > setPermanentCookie doc =<< defaultCookie "key" (Just "value")
setPermanentCookie :: (MonadJSM m) => DOM.Document -> SetCookie -> m ()
setPermanentCookie doc cookie = do
  DOM.setCookie doc $ decodeUtf8 $ LBS.toStrict $ toLazyByteString $ renderSetCookie $ base64EncodeCookie cookie

-- | Set or clear the given cookie with given expiration date
--
-- Example:
-- > setExpiringCookie time doc =<< defaultCookie "key" (Just "value")
setExpiringCookie :: (MonadJSM m) => UTCTime -> DOM.Document -> SetCookie -> m ()
setExpiringCookie timestamp doc cookie = do
  DOM.setCookie doc $ decodeUtf8 $ LBS.toStrict $ toLazyByteString $ renderSetCookie $ base64EncodeCookie cookie {setCookieExpires = Just timestamp}

-- | Make a cookie with sensible defaults
defaultCookie
  :: (MonadJSM m)
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

-- | JSON encode some data and set it as a cookie
defaultCookieJson :: (MonadJSM m, ToJSON v) => Text -> Maybe v -> m SetCookie
defaultCookieJson k = defaultCookie k . fmap (decodeUtf8 . LBS.toStrict . encode)

-- | Set a cookie with the given domain location: see
-- <https://developer.mozilla.org/en-US/docs/web/api/document/cookie documentation>
-- for @;domain=domain@
setPermanentCookieWithLocation :: (MonadJSM m) => DOM.Document -> Maybe ByteString -> Text -> Maybe Text -> m ()
setPermanentCookieWithLocation doc loc key mv = do
  cookie <- defaultCookie key mv
  setPermanentCookie doc $ cookie { setCookieDomain = loc }

data GetCookieFailed
  = GetCookieFailed_NotFound
  | GetCookieFailed_Base64DecodeFailed String

-- | Retrieve the value of the given cookie
getCookie :: MonadJSM m => DOM.Document -> Text -> m (Either GetCookieFailed Text)
getCookie doc key = do
  cookieString <- encodeUtf8 <$> DOM.getCookie doc
  pure $ case lookup (encodeUtf8 key) $ parseCookies cookieString of
    Nothing -> Left GetCookieFailed_NotFound
    Just c -> mapBoth GetCookieFailed_Base64DecodeFailed decodeUtf8 $
      base64Decode c

-- | JSON encode some data and set it as a permanent cookie
setPermanentCookieJson :: (MonadJSM m, ToJSON v) => DOM.Document -> Text -> Maybe v -> m ()
setPermanentCookieJson d k = setPermanentCookie d <=< defaultCookieJson k

data GetCookieJsonFailed
  = GetCookieJsonFailed_GetCookieFailed GetCookieFailed
  | GetCookieJsonFailed_ParseFailure String

-- | Read a cookie. You may want to use 'Obelisk.Frontend.Cookie.askCookies'
-- along with 'base64Decode' instead.
getCookieJson :: (FromJSON v, MonadJSM m) => DOM.Document -> Text -> m (Either GetCookieJsonFailed v)
getCookieJson d k = do
  r <- fmap (eitherDecode . LBS.fromStrict . encodeUtf8) <$> getCookie d k
  pure $ case r of
    Left failure -> Left $ GetCookieJsonFailed_GetCookieFailed failure
    Right (Left parseFailure) -> Left $ GetCookieJsonFailed_ParseFailure parseFailure
    Right (Right v) -> Right v

-- | Get a cookie and run an action on it. Set the cookie value to the result
-- of the action.
withPermanentCookieJson ::
  ( MonadJSM m
  , MonadJSM (Performable m)
  , PerformEvent t m
  , ToJSON v
  , FromJSON v
  )
  => DOM.Document
  -> Text
  -> (Either GetCookieJsonFailed v -> m (Event t (Maybe v)))
  -> m ()
withPermanentCookieJson d k a = do
  cookie0 <- getCookieJson d k
  cookieE <- a cookie0
  performEvent_ $ setPermanentCookieJson d k <$> cookieE
  return ()
