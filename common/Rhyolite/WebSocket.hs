{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.WebSocket where

import Data.Aeson
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Network.URI (URI(..))
import Rhyolite.App
import Rhyolite.Request.Class

websocketUri :: URI -> URI
websocketUri uri = uri
  { uriScheme = case uriScheme uri of
    "http:" -> "ws:"
    "https:" -> "wss:"
    "file:" -> "ws:"
    p -> error $ "Unrecognized protocol: " <> p
  }

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
data TaggedRequest r = TaggedRequest Int (SomeRequest r)
  deriving (Typeable, Generic)

instance Request r => FromJSON (TaggedRequest r)
instance Request r => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Int Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse
