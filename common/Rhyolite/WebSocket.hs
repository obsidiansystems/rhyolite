-- | The low-level implementation of the websocket communication between
-- frontend and backend. You can use this manually, for example, when building a
-- program that pretends to be a user of your app (load-testing comes to mind).

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
import Data.Typeable
import GHC.Generics
import Network.URI (URI(..))
import Reflex.Query.Class

websocketUri :: URI -> URI
websocketUri uri = uri
  { uriScheme = case uriScheme uri of
    "http:" -> "ws:"
    "https:" -> "wss:"
    "file:" -> "ws:"
    p -> error $ "Unrecognized protocol: " <> p
  }

-- | Represents a WebSocket message from one of two channels: ViewSelector declarations or API requests
data WebSocketRequest q r = WebSocketRequest_ViewSelector q
                          | WebSocketRequest_Api (TaggedRequest r)
  deriving (Typeable, Generic)

instance (FromJSON q, FromJSON (Some r)) => FromJSON (WebSocketRequest q r)
instance (ToJSON q, ToJSON (Some r)) => ToJSON (WebSocketRequest q r)

-- | Represents a WebSocket response from one of three channels: incoming 'View's, API responses, or version info
data WebSocketResponse q = WebSocketResponse_View (QueryResult q)
                         | WebSocketResponse_Api TaggedResponse
                         | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance FromJSON (QueryResult q) => FromJSON (WebSocketResponse q)
instance ToJSON (QueryResult q) => ToJSON (WebSocketResponse q)

-- | A request tagged with an identifier
data TaggedRequest r = TaggedRequest Int (Some r)
  deriving (Typeable, Generic)

instance FromJSON (Some r) => FromJSON (TaggedRequest r)
instance ToJSON (Some r) => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Int Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse
