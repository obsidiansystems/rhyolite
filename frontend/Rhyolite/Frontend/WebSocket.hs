{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rhyolite.Frontend.WebSocket where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Types
import Reflex.Dom.Core
import qualified Reflex.Dom.WebSocket as RDWS
#if defined(ghcjs_HOST_OS)
import Control.Exception
import qualified Data.Aeson as Aeson
import Data.Aeson.Types
import GHCJS.Marshal
import GHCJS.Types (JSVal)
import System.IO.Unsafe
#endif

import Rhyolite.WebSocket

newtype JSWebSocket x = JSWebSocket { unWebSocket :: JSRef x }

instance ToJS x (JSWebSocket x) where
  withJS (JSWebSocket r) f = f r

instance FromJS x (JSWebSocket x) where
  fromJS = return . JSWebSocket

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
webSocket
  :: forall x t m. (HasJS x m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadJSM m, MonadJSM (Performable m), HasJSContext m)
  => Either WebSocketUrl Text
  -> WebSocketConfig t Text
  -> m (WebSocket t)
webSocket murl config
  | Left url <- murl =
    RDWS.webSocket (mconcat [ _websocket_protocol url, "://"
                            , _websocket_host url, ":", T.pack (show (_websocket_port url))
                            , "/", _websocket_path url ]) config
  | Right path <- murl = do
    pageHost <- getLocationHost
    pageProtocol <- getLocationProtocol
    let wsProtocol = case pageProtocol of
          "http:" -> "ws:"
          "https:" -> "wss:"
          "file:" -> "ws:"
          s -> error $ "unrecognized wsProtocol: " <> T.unpack s
        wsHost = case pageProtocol of
          "file:" -> "localhost:8000"
          _ -> pageHost
    RDWS.webSocket (wsProtocol <> "//" <> wsHost <> path) config

#if defined(ghcjs_HOST_OS)
rawWebSocket
  :: forall x t m. (HasJS x m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadJSM m, MonadJSM (Performable m), HasJSContext m)
  => Either WebSocketUrl Text
  -> WebSocketConfig t Text
  -> m (RawWebSocket t JSVal)
rawWebSocket murl config
  | Left url <- murl
  = do
    RDWS.webSocket' (mconcat [ _websocket_protocol url, "://"
                             , _websocket_host url, ":", T.pack (show (_websocket_port url))
                             , "/", _websocket_path url ]) config (either (error "websocket': expected JSVal") return)
  | Right path <- murl = do
    pageHost <- liftJSM getLocationHost
    pageProtocol <- liftJSM getLocationProtocol
    let wsProtocol = case pageProtocol of
          "http:" -> "ws:"
          "https:" -> "wss:"
          "file:" -> "ws:"
          s -> error $ "unrecognized wsProtocol: " <> T.unpack s
        wsHost = case pageProtocol of
          "file:" -> "localhost:8000"
          _ -> pageHost
    RDWS.webSocket' (wsProtocol <> "//" <> wsHost <> path) config (either (error "websocket': expected JSVal") return)

foreign import javascript unsafe "JSON['parse']($1)" js_jsonParse :: JSVal -> JSVal

rawDecode :: (FromJSON a) => JSVal -> Maybe a
rawDecode jsv = do
  -- traceM "customDecode"
  -- TODO pFromJSVal to avoid unsafePerformIO
  let res = unsafePerformIO $ try $ fromJSVal $ js_jsonParse jsv
  case res of
   Left (_e::SomeException) -> do
     -- traceM $ "====================================================================="
     -- traceM $ show e
     -- traceM $ "====================================================================="
     Nothing
   Right (v :: (Maybe Aeson.Value)) -> do
     -- traceM $ show $ js_jsonTypeOf jsv'
     -- traceM $ "Success" ++ show v
     maybe Nothing go v
  where
    go v = case Aeson.fromJSON v of
      Aeson.Success a -> Just a
      _ -> Nothing
#endif
