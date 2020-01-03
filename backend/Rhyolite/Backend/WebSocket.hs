-- | Setting up the WebSocket connection using "Snap.Core" and primitives for
-- getting and setting websocket messages.

module Rhyolite.Backend.WebSocket where

import Data.Semigroup ((<>))
import Control.Exception (SomeException (..), handle, throwIO, AssertionFailed (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets.Stream as WS
import Network.WebSockets.Snap (runWebSocketsSnap)
import Snap.Core (MonadSnap)

-- | Accepts a websockets connection and runs the supplied action with it using the given logging function
--   when an error occurs.
withWebsocketsConnectionLogging :: MonadSnap m => (String -> SomeException -> IO ()) -> (WS.Connection -> IO ()) -> m ()
withWebsocketsConnectionLogging logger f = runWebSocketsSnap $ withPendingWebsocketConnection logger f

-- | Like 'withWebsocketsConnectionLogging' but with a default logging function using 'putStrLn'.
withWebsocketsConnection :: MonadSnap m => (WS.Connection -> IO ()) -> m ()
withWebsocketsConnection f = runWebSocketsSnap $ withPendingWebsocketConnection logger f
  where
    logger str e = putStrLn $ "withWebsocketsConnection: " <> (if null str then "" else str <> ": ") <> show e

withPendingWebsocketConnection :: (String -> SomeException -> IO ()) -> (WS.Connection -> IO ()) -> WS.PendingConnection -> IO ()
withPendingWebsocketConnection logger f pc = do
  conn <- WS.acceptRequest pc
  handleSomeException $ handleConnectionException $ f conn
  where
    handleSomeException = handle $ \e -> logger "" e
    handleConnectionException = handle $ \e -> case e of
      WS.ConnectionClosed -> return ()
      WS.CloseRequest _ _ -> print e >> WS.close (WS.pendingStream pc) >> throwIO e
      _ -> logger "Exception" (SomeException e) *> throwIO e


-- | Attempts to json decode a websockets data message
decodeWebsocketsDataMessage :: FromJSON a => WS.DataMessage -> Either String a
decodeWebsocketsDataMessage dm = eitherDecode' $ case dm of
  WS.Text r' _ -> r'
  WS.Binary r' -> r'

-- | Parse and process a single websocket data message
getDataMessage
  :: FromJSON a
  => WS.Connection
  -> IO a
getDataMessage conn = do
  dm <- WS.receiveDataMessage conn
  case decodeWebsocketsDataMessage dm of
    Left err -> liftIO $ throwIO $ AssertionFailed $ mconcat
      [ "getDataMessage: error: "
      , err
      , "; received: "
      , show dm
      ]
    Right a -> return a

-- | Send a json encoded data message over the websocket connection
sendEncodedDataMessage
  :: ToJSON a
  => WS.Connection
  -> a
  -> IO ()
sendEncodedDataMessage conn = WS.sendDataMessage conn . (\x -> WS.Text x Nothing) . encode
