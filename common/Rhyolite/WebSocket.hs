{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.WebSocket where

import Data.Aeson
import Data.Typeable
import Data.Text (Text)
import qualified Data.Text as T
import Rhyolite.App
import Rhyolite.Request.Class
import Rhyolite.Route
import GHC.Generics
import Text.Read (readMaybe)

data WebSocketUrl = WebSocketUrl
  { _websocket_protocol :: Text
  , _websocket_host :: Text
  , _websocket_port :: Int
  , _websocket_path :: Text
  } deriving (Eq, Ord, Show, Read)

websocketUrlFromRouteEnv :: RouteEnv -> WebSocketUrl
websocketUrlFromRouteEnv (protocol, host, port) =
  let  (wsProtocol, wsPortDef) = case protocol of
         "http:" -> ("ws", 80)
         "https:" -> ("wss", 443)
         "file:" -> ("ws", 80)
         _ -> error "Unrecognized protocol"
       wsPort = case readMaybe . T.unpack =<< T.stripPrefix ":" (T.pack port) of
                     Nothing -> wsPortDef
                     Just n -> n
  in WebSocketUrl wsProtocol (T.pack host) wsPort "listen"

-- | Represents a WebSocket message from one of two channels: ViewSelector declarations or API requests
data WebSocketRequest app r = WebSocketRequest_ViewSelector (ViewSelector app ())
                            | WebSocketRequest_Api (TaggedRequest r)
  deriving (Typeable, Generic)

instance (Request r, FromJSON (ViewSelector app ())) => FromJSON (WebSocketRequest app r)
instance (Request r, ToJSON (ViewSelector app ())) => ToJSON (WebSocketRequest app r)

-- | Represents a WebSocket response from one of three channels: incoming 'View's, API responses, or version info
data WebSocketResponse app = WebSocketResponse_View (View app ())
                           | WebSocketResponse_Api TaggedResponse
                           | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance FromJSON (View app ()) => FromJSON (WebSocketResponse app)
instance ToJSON (View app ()) => ToJSON (WebSocketResponse app)

-- | A request tagged with an identifier
data TaggedRequest r = TaggedRequest Value (SomeRequest r)
  deriving (Typeable, Generic)

instance Request r => FromJSON (TaggedRequest r)
instance Request r => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Value Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse
