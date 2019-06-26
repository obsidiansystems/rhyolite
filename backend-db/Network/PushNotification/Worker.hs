{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.PushNotification.Worker where

import Control.Concurrent
import Control.Monad
import Control.Monad.Logger
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Pool
import Database.Groundhog
import Database.Groundhog.TH
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB
import Rhyolite.Backend.Schema
import Rhyolite.Backend.Schema.TH
import Rhyolite.Concurrent
import Rhyolite.Schema
import GHC.Generics

import Network.HTTP.Conduit

import Network.PushNotification.Android
import Network.PushNotification.Android.Payload
import Network.PushNotification.IOS

data APNSServer = APNSServer String
  deriving (Show, Generic)

instance ToJSON APNSServer
instance FromJSON APNSServer

instance HasId ApplePushMessage

mkRhyolitePersist (Just "migrateApplePushMessage") [groundhog|
  - entity: ApplePushMessage
|]

makeDefaultKeyIdInt64 ''ApplePushMessage 'ApplePushMessageKey

data AndroidPushMessage = AndroidPushMessage
  { _androidPushMessage_payload :: Json FcmPayload }
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
  => APNSConfig
  -> Int
  -> f (Pool Postgresql)
  -> m (IO ())
apnsWorker cfg delay db = askLoggerIO >>= \logger -> return . killThread <=<
  liftIO . forkIO . supervise . liftIO . withAPNSSocket cfg $ \conn -> do
    void $ forever $ do
      let clear = do
            qm <- Map.toList <$>
              selectMap ApplePushMessageConstructor (CondEmpty `limitTo` 1)
            case qm of
              [(k, m)] -> do
                if LBS.length (_applePushMessage_payload m) > maxPayloadLength
                  then deleteBy (fromId k) >> clear
                  else do
                    liftIO $ sendApplePushMessage conn m
                    deleteBy $ fromId k
                    clear
              _ -> return ()
      runLoggingT (runDb db clear >> liftIO (threadDelay delay)) logger
    return ()

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
