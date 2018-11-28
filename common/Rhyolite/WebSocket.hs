{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.WebSocket where

import Data.Aeson
import Data.Semigroup ((<>))
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import Rhyolite.Route
import Text.Read (readMaybe)
import Reflex (Query(..))

data WebSocketUrl = WebSocketUrl
  { _websocket_protocol :: Text
  , _websocket_host :: Text
  , _websocket_port :: Int
  , _websocket_path :: Text
  } deriving (Eq, Ord, Show, Read)

websocketUrlFromRouteEnv :: RouteEnv -> WebSocketUrl
websocketUrlFromRouteEnv = websocketUrlFromRouteEnv' "listen"

websocketUrlFromRouteEnv' :: Text -> RouteEnv -> WebSocketUrl
websocketUrlFromRouteEnv' path (protocol, host, port) =
  let  (wsProtocol, wsPortDef) = case protocol of
         "http:" -> ("ws", 80)
         "https:" -> ("wss", 443)
         "file:" -> ("ws", 80)
         p -> error $ "Unrecognized protocol: " <> p
       wsPort = case readMaybe . T.unpack =<< T.stripPrefix ":" (T.pack port) of
                     Nothing -> wsPortDef
                     Just n -> n
  in WebSocketUrl wsProtocol (T.pack host) wsPort path

-- | Represents a WebSocket message from one of two channels: ViewSelector declarations or API requests
data WebSocketRequest q r = WebSocketRequest_ViewSelector q
                          | WebSocketRequest_Api (TaggedRequest r)
  deriving (Typeable, Generic)

instance (FromJSON (Some r), FromJSON q) => FromJSON (WebSocketRequest q r)
instance (ToJSON (Some r), ToJSON q) => ToJSON (WebSocketRequest q r)

-- | Represents a WebSocket response from one of three channels: incoming 'View's, API responses, or version info
data WebSocketResponse q = WebSocketResponse_View (QueryResult q)
                         | WebSocketResponse_Api TaggedResponse
                         | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance FromJSON (QueryResult q) => FromJSON (WebSocketResponse q)
instance ToJSON (QueryResult q) => ToJSON (WebSocketResponse q)

-- | A request tagged with an identifier
data TaggedRequest r = TaggedRequest Value (Some r)
  deriving (Typeable, Generic)

instance (FromJSON (Some r)) => FromJSON (TaggedRequest r)
instance (ToJSON (Some r)) => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Value Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse
