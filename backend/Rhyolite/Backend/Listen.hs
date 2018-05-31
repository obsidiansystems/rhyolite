{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Rhyolite.Backend.Listen where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON, Value, encode, toJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.List.Split (wordsBy)
import Data.Functor.Identity (Identity (..))
import Data.Pool (Pool, withResource)
import Data.Semigroup ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Database.Groundhog.Core as GH
import Database.Groundhog.Core
import qualified Database.Groundhog.Expression as GH
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import GHC.Generics (Generic)

import Rhyolite.Backend.DB (getSearchPath)
import Rhyolite.Backend.Schema (fromId, toId)
import Rhyolite.Backend.Schema.Class (DefaultKeyId)
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Request.TH (makeJson)
import Rhyolite.Schema (SchemaName (..), Id, IdData)


data NotifyMessage = NotifyMessage
  { _notifyMessage_schemaName :: SchemaName
  , _notifyMessage_notificationType :: NotificationType
  , _notifyMessage_entityName :: String
  , _notifyMessage_value :: Value
  } deriving (Show, Read, Eq, Generic)

instance FromJSON NotifyMessage
instance ToJSON NotifyMessage

data NotificationType = NotificationType_Insert
                      | NotificationType_Update
                      | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | The channel name used for insert/update/delete notifications
notifyChannel :: String
notifyChannel = "updates"

-- | Starts a thread to listen for updates to the database
notificationListener
  :: Pool Postgresql
  -> IO (TChan NotifyMessage, IO ())
notificationListener db = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withResource db $ \(Postgresql conn) -> do
    let cmd = "LISTEN " <> notifyChannel
    _ <- PG.execute_ conn $ fromString cmd
    forever $ do
      -- Handle notifications
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel == encodeUtf8 (T.pack notifyChannel) -> do -- Notification is on notifyChannel
          case decodeValue' $ LBS.fromStrict message of
            Just a -> atomically $ writeTChan nChan a
            _ -> putStrLn $ "listenDB: Could not parse message on updates channel: " <> show message
        _ -> putStrLn $ "listenDB: Received a message on unexpected channel: " <> show channel
  return (nChan, killThread daemonThread)

-- | Starts a thread that listens for updates to the db and returns a
-- 'NotifyMessage' retrieval function and finalizer
startNotificationListener :: Pool Postgresql -> IO (IO NotifyMessage, IO ())
startNotificationListener pool = do
  (chan, nkill) <- notificationListener pool
  chan' <- atomically $ dupTChan chan
  return (atomically $ readTChan chan', nkill)

insertAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Id a)
insertAndNotify t = do
  tid <- fmap toId $ insert t
  notifyEntityId NotificationType_Insert tid
  return tid

insertAndNotify_ :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m ()
insertAndNotify_ = void . insertAndNotify

insertByAllAndNotify :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
insertByAllAndNotify t = do
  etid <- fmap (fmap toId) $ insertByAll t
  case etid of
    Left _ -> return Nothing
    Right tid -> notifyEntityId NotificationType_Insert tid >> return (Just tid)

insertByAllAndNotifyWithBody :: (PersistBackend m, DefaultKey a ~ AutoKey a, DefaultKeyId a, PersistEntity a, ToJSON a, ToJSON (IdData a)) => a -> m (Maybe (Id a))
insertByAllAndNotifyWithBody t = do
  etid <- fmap (fmap toId) $ insertByAll t
  case etid of
    Left _ -> return Nothing
    Right tid -> notifyEntityWithBody NotificationType_Insert tid t >> return (Just tid)

--TODO: remove type hole from signature; may need to modify groundhog to make that possible
updateAndNotify :: (ToJSON (IdData a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistEntity a, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), DefaultKeyId a, _)
                => Id a
                -> [GH.Update (PhantomDb m) (RestrictionHolder v c)]
                -> m ()
updateAndNotify tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  notifyEntityId NotificationType_Update tid

updateAndNotifyWithBody :: (ToJSON (IdData a), GH.Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a), PersistEntity v, PersistEntity a, PersistBackend m, GH.Unifiable (AutoKeyField v c) (DefaultKey a), DefaultKeyId a, _)
                        => Id a
                        -> [GH.Update (PhantomDb m) (RestrictionHolder v c)]
                        -> m ()
updateAndNotifyWithBody tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  Just t <- get $ fromId tid
  notifyEntityWithBody NotificationType_Update tid t

getSchemaName :: PersistBackend m
              => m String
getSchemaName =  do
  searchPath <- getSearchPath
  let searchPathComponents = wordsBy (==',') searchPath
      schemaName = case searchPathComponents of
       (x:_:_:_) -> x
       _ -> "public"
  return schemaName

notifyEntityWithBody' :: forall a b m. (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON b) => NotificationType -> Id a -> b -> m ()
notifyEntityWithBody' nt aid b = notifyEntities nt (b, aid)

notifyEntityWithBody :: (PersistBackend m, PersistEntity a, ToJSON (IdData a), ToJSON a) => NotificationType -> Id a -> a -> m ()
notifyEntityWithBody = notifyEntityWithBody'

notifyEntityId :: forall a m. (PersistBackend m, PersistEntity a, ToJSON (IdData a)) => NotificationType -> Id a -> m ()
notifyEntityId nt aid = notifyEntities nt (Identity aid)

notifyEntities :: forall a f m. (PersistBackend m, PersistEntity a, ToJSON (f (Id a))) => NotificationType -> f (Id a) -> m ()
notifyEntities nt aid = do
  schemaName <- getSchemaName
  let proxy = undefined :: proxy (PhantomDb m)
      cmd = "NOTIFY " <> notifyChannel <> ", ?"
      notification = NotifyMessage { _notifyMessage_schemaName = SchemaName . T.pack $ schemaName
                                   , _notifyMessage_notificationType = nt
                                   , _notifyMessage_entityName = entityName $ entityDef proxy (undefined :: a)
                                   , _notifyMessage_value = toJSON aid
                                   }
  _ <- executeRaw False cmd [PersistString $ T.unpack $ decodeUtf8 $ LBS.toStrict $ encode notification]
  return ()


makeJson ''NotificationType
