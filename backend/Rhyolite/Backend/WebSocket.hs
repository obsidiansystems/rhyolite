{-|
Description: Websocket connection, send, and receive

Setting up the WebSocket connection using 'Snap.Core' and primitives for
getting and setting websocket messages.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
module Rhyolite.Backend.WebSocket where

import qualified Network.WebSockets as WS
import Snap.Core (MonadSnap)
import Network.WebSockets.Snap (runWebSocketsSnapWith)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Functor
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Exception (SomeException (..), throw, finally, try, evaluate)
import Data.Typeable (cast)
import Control.Concurrent.Async
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Word
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import qualified Data.Text as T

-- Start processing a stream of messages to produce responses
-- Notes:
-- - There is no way to request the stream to close
-- - When the stream is closed:
--   - `send` simply does nothing and returns immediately
--   - `recv` simply hangs (waiting for a message that will never come)
--   - the returned IO () will be invoked
-- - It is OK to invoke `send` or `recv` before returning from the initial IO action
type Processor a b = (b -> IO (), IO a) -> IO (IO ())

sendThisFirst :: b -> Processor a b -> Processor a b
sendThisFirst initialVal p (send, recv) = do
  send initialVal
  p (send, recv)

processByteStrings :: Processor ByteString ByteString -> Processor (Either ByteString Text) (Either ByteString Text)
processByteStrings p (send, recv) = do
  p ( send . Left
    , recv <&> \case
        Left bs -> bs
        Right t -> encodeUtf8 t
    )

processInputJson
  :: FromJSON a
  => (Text -> ByteString -> b) -- ^ Construct a reply to send when a JSON parsing error occurs
  -> Processor a b
  -> Processor ByteString b
processInputJson formatError p (send, recv) = do
  p ( send
    , let recv' = recv >>= \raw -> case eitherDecodeStrict' raw of
            Left err -> do
              send $ formatError (T.pack err) raw -- Inform the client of the problem
              recv' -- Wait for the next valid input
            Right a -> do
              pure a
      in recv'
    )

processOutputJson
  :: ToJSON b
  => Processor a b
  -> Processor a ByteString
processOutputJson p (send, recv) = do
  p ( send . LBS.toStrict . encode
    , recv
    )

processJson
  :: forall a b
  .  ( FromJSON a
     , ToJSON b
     )
  => (Text -> ByteString -> b)
  -> Processor a b
  -> Processor ByteString ByteString
processJson formatError = processOutputJson . processInputJson formatError

echo :: Processor a a
echo (send, recv) = do
  thread <- async $ forever $ send =<< recv
  pure $ cancel thread

data WebSocketState
   = WebSocketState_Open WS.Connection
   | WebSocketState_Closed

serveWebSocket :: MonadSnap m => Processor (Either ByteString Text) (Either ByteString Text) -> m ()
serveWebSocket p = do
  let connOptions = WS.defaultConnectionOptions
        { WS.connectionStrictUnicode = False -- We don't want exceptions
        }
  runWebSocketsSnapWith connOptions $ \pendingConn -> do
    --TODO: Allocate a random connection ID for logging purposes
    origConn <- WS.acceptRequest pendingConn --TODO: Log that we started a connection
    let origState = WebSocketState_Open origConn
    state <- newMVar origState
    stateForRecv <- newIORef origState -- This is separate so that there is not contention between send and recv; it is fine for this value to be delayed compared to the contents of `state`
    done <- newEmptyMVar
    let send :: Either ByteString Text -> IO ()
        send msg = do
          preparedMsg <- case msg of
            Left bs -> do
              preparedBs <- evaluate bs -- If this causes exceptions, they will be thrown to the caller, who is responsible for giving us this message
              pure $ WS.Binary $ LBS.fromStrict preparedBs
            Right t -> do
              preparedBs <- evaluate (encodeUtf8 t) -- If this causes exceptions, they will be thrown to the caller, who is responsible for giving us this message
              pure $ WS.Text (LBS.fromStrict preparedBs) Nothing
          result <- withMVar state $ \case
            WebSocketState_Closed -> pure $ Right () --TODO: Log server-side; this means something is still sending us messages after we have closed, which is not great
            WebSocketState_Open conn -> try $ WS.sendDataMessage conn preparedMsg
          case result of
            Left (SomeException e)
              | Just (e' :: WS.ConnectionException) <- cast e -> case e' of
                  WS.CloseRequest _ _ -> void markClosed
                  WS.ConnectionClosed -> void markClosed
                  WS.ParseException _ -> sendYourFaultClose --TODO: Log the exception server-side
                  WS.UnicodeException _ -> sendMyFaultClose --TODO: Should never happen; Log server-side
              | otherwise -> throw e
            Right () -> pure ()
        recv :: IO (Either ByteString Text)
        recv = readIORef stateForRecv >>= \case
          WebSocketState_Closed -> blockForever
          WebSocketState_Open conn -> try (WS.receiveDataMessage conn) >>= \case
            Left (SomeException e)
              | Just (e' :: WS.ConnectionException) <- cast e -> case e' of
                  WS.CloseRequest _ _ -> markClosed >> blockForever -- The other side has closed the connection (cleanly); therefore, we will never get another message
                  WS.ConnectionClosed -> markClosed >> blockForever -- The other side has closed the connection (uncleanly); therefore, we will never get another message
                  WS.ParseException _ -> sendYourFaultClose >> blockForever --TODO: Log server-side
                  WS.UnicodeException _ -> sendMyFaultClose >> blockForever --TODO: Should never happen; Log server-side
              | otherwise -> throw e
            Right msg -> pure $ case msg of
              WS.Binary bs -> Left $ LBS.toStrict bs
              WS.Text bs _ -> Right $ decodeUtf8With lenientDecode $ LBS.toStrict bs
        blockForever = forever $ threadDelay maxBound
        markClosed :: IO WebSocketState
        markClosed = do
          oldState <- swapMVar state WebSocketState_Closed
          case oldState of
            WebSocketState_Closed -> pure ()
            WebSocketState_Open _ -> do
              writeIORef stateForRecv WebSocketState_Closed
          pure oldState
        sendCloseCode :: Word16 -> ByteString -> IO ()
        sendCloseCode code text = markClosed >>= \case
          WebSocketState_Closed -> pure ()
          WebSocketState_Open conn -> do
            WS.sendCloseCode conn code text
            forkIO $ forever $ do
              _ <- WS.receiveDataMessage conn -- Clear all connections
              pure ()
            _ <- tryPutMVar done () --TODO: Log server-side if this returns False; that means we already thought we were done
            pure ()
        sendYourFaultClose = sendCloseCode 1002 "protocol error"
        sendMyFaultClose = sendCloseCode 1011 "internal error"
        sendGoingAwayClose = sendCloseCode 1001 "going away"
    shutdownP <- p (send, recv)
    takeMVar done `finally` do
      sendGoingAwayClose
      shutdownP
