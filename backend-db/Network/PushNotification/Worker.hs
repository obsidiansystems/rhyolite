{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.PushNotification.Worker where

import Control.Concurrent
import Control.Lens ((<&>))
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.Function (fix)
import qualified Data.Map as Map
import Data.Pool
import Data.Time (UTCTime)
import Data.Word (Word32)
import Database.Groundhog
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Database.Id.Class
import Database.Id.Groundhog
import Database.Id.Groundhog.TH
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple (queryQ)
import Rhyolite.Backend.Schema.TH
import Rhyolite.Concurrent
import Rhyolite.Schema
import GHC.Generics

import Network.HTTP.Conduit

import Network.PushNotification.Android
import Network.PushNotification.Android.Payload
import qualified Network.PushNotification.IOS as IOS

data APNSServer = APNSServer String
  deriving (Show, Generic)

instance ToJSON APNSServer
instance FromJSON APNSServer

-- | A copy of 'IOS.ApplePushMessage' but with additional fields for managing the queue.
data ApplePushMessage = ApplePushMessage
  { _applePushMessage_deviceToken :: !ByteString
  , _applePushMessage_payload :: !LBS.ByteString
  , _applePushMessage_expiry :: !Word32
  , _applePushMessage_claimedAt :: !(Maybe UTCTime)
  } deriving (Eq, Generic, Ord, Show)
instance HasId ApplePushMessage

mkRhyolitePersist (Just "migrateApplePushMessage") [groundhog|
  - entity: ApplePushMessage
|]

makeDefaultKeyIdInt64 ''ApplePushMessage 'ApplePushMessageKey

queueApplePushMessage :: PersistBackend m => IOS.ApplePushMessage -> m (Id ApplePushMessage)
queueApplePushMessage x = fmap toId $ insert $ ApplePushMessage
  { _applePushMessage_deviceToken = IOS._applePushMessage_deviceToken x
  , _applePushMessage_payload = IOS._applePushMessage_payload x
  , _applePushMessage_expiry = IOS._applePushMessage_expiry x
  , _applePushMessage_claimedAt = Nothing
  }

data AndroidPushMessage = AndroidPushMessage
  { _androidPushMessage_payload :: Json FcmPayload
  }
  deriving (Generic)

instance ToJSON AndroidPushMessage
instance FromJSON AndroidPushMessage
instance HasId AndroidPushMessage

mkRhyolitePersist (Just "migrateAndroidPushMessage") [groundhog|
  - entity: AndroidPushMessage
|]
makeDefaultKeyIdInt64 ''AndroidPushMessage 'AndroidPushMessageKey

apnsWorker
  :: (MonadLoggerIO m, RunDb f)
  => IOS.APNSConfig
  -> Int
  -> f (Pool Postgresql)
  -> m (IO ())
apnsWorker cfg delay db = askLoggerIO >>= \logger -> return . killThread <=<
  liftIO . forkIO . supervise . liftIO . IOS.withAPNSSocket cfg $ \apnsConn -> do
    void $ flip runLoggingT logger $ forever $ do
      fix $ \loop -> do
        processApplePushMessage db (IOS.sendApplePushMessage apnsConn) >>= \case
          Just ProcessingDone -> pure ()
          Nothing -> loop
      liftIO $ threadDelay delay

data ProcessingDone = ProcessingDone deriving (Eq, Show)

-- | Process a single 'ApplePushMessage' from the queue.
-- Guarantees "at most once" semantics, e.g. if there is a power fault
-- then some notifications may not be sent, but nothing will ever be
-- sent twice.
processApplePushMessage
  :: (MonadLoggerIO m, RunDb f)
  => f (Pool Postgresql)
  -> (IOS.ApplePushMessage -> IO ())
  -> m (Maybe ProcessingDone)
processApplePushMessage db sendMessage = do
  msg' <- runDb db $ do
    -- NB: There may be claimed notifications that are simply never sent.
    -- This is consistent with "at most once".
    qm <-
      [queryQ|
        SELECT "id", "deviceToken", "payload", "expiry"
        FROM "ApplePushMessage"
        WHERE "claimedAt" IS NULL
        LIMIT 1
      |] <&> fmap (\(mid, deviceToken, payload, expiry :: Int) -> (mid :: Id ApplePushMessage, IOS.ApplePushMessage
        { IOS._applePushMessage_deviceToken = deviceToken
        , IOS._applePushMessage_payload = payload
        , IOS._applePushMessage_expiry = fromIntegral expiry
        }))
    case qm of
      [] -> pure Nothing
      [(k, m)] -> do
        if LBS.length (IOS._applePushMessage_payload m) > IOS.maxPayloadLength
          then Nothing <$ deleteBy (fromId k)
          else do
            now <- getTime
            _ <- update [ApplePushMessage_claimedAtField =. Just now] (AutoKeyField `in_` [fromId k])
            pure $ Just (k, m)

      _ -> error "processApplePushMessage: Received multiple row despite LIMIT 1"

  for_ msg' $ \(k, m) -> do
    liftIO $ sendMessage m
    runDb db $ deleteBy (fromId k)

  -- If @msg'@ is empty then we're done.
  pure $ maybe (Just ProcessingDone) (const Nothing) msg'


firebaseWorker
  :: (MonadLoggerIO m, RunDb f)
  => ByteString -- ^ Firebase server key
  -> Int -- ^ Sleep length when queue is clear (seconds)
  -> f (Pool Postgresql) -- ^ DB pool
  -> m (IO ()) -- ^ IO Action to kill thread
firebaseWorker key delay db = askLoggerIO >>= \logger -> do
  mgr <- liftIO $ newManager tlsManagerSettings
  worker delay $ do
    let clear = do
          p <- runDb db $ Map.toList <$>
            selectMap AndroidPushMessageConstructor (CondEmpty `limitTo` 1)
          case p of
            [(k, AndroidPushMessage (Json m))] -> do
              _ <- liftIO $ sendAndroidPushMessage mgr key m
              runDb db $ deleteBy $ fromId k
              clear
            _ -> return ()
    runLoggingT clear logger
