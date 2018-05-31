module Rhyolite.Backend.WebSocket where

import Data.Semigroup
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import Network.WebSockets.Snap
import Snap.Core (MonadSnap)

-- | Accepts a websockets connection and runs the supplied action with it
withWebsocketsConnection :: MonadSnap m => (WS.Connection -> IO ()) -> m ()
withWebsocketsConnection f = runWebSocketsSnap $ \pc -> do
  conn <- WS.acceptRequest pc
  handleSomeException $ handleConnectionException pc $ f conn
  where
    handleSomeException = handle $ \(SomeException e) -> putStrLn $ "withWebsocketsConnection: " <> displayException e
    handleConnectionException pc = handle $ \e -> case e of
      WS.ConnectionClosed -> return ()
      WS.CloseRequest _ _ -> print e >> WS.pendingStreamClose pc >> throwIO e
      _ -> do putStr $ "withWebsocketsConnection: Exception: " <> displayException e
              throwIO e

-- | Attempts to json decode a websockets data message
decodeWebsocketsDataMessage :: FromJSON a => WS.DataMessage -> Either String a
decodeWebsocketsDataMessage dm = eitherDecode' $ case dm of
  WS.Text r' -> r'
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
sendEncodedDataMessage conn = WS.sendDataMessage conn . WS.Text . encode
