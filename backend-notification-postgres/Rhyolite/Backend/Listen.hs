-- | Notification mechanism in the backend. The most important function in this
-- module is 'notify'.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Rhyolite.Backend.Listen where

import Gargoyle.PostgreSQL
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity (..))
import Data.List.Split (wordsBy)
import Data.Pool (Pool, withResource)
import Data.Some (Some)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import GHC.Generics (Generic)

import Rhyolite.Backend.DB (getSearchPath)
import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Schema (SchemaName (..))

data NotificationType = NotificationType_Insert
                      | NotificationType_Update
                      | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON NotificationType
instance FromJSON NotificationType

-- | The channel name used for insert/update/delete notifications
notifyChannel :: String
notifyChannel = "updates"

-- | Starts a thread to listen for updates to the database
notificationListener
  :: (FromJSON notifyMessage)
  => Pool PG.Connection
  -> IO (TChan notifyMessage, IO ()) -- "notifyMessage" is usually a DbNotification
notificationListener db = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withResource db $ \conn -> do
    let cmd = "LISTEN " <> notifyChannel
    _ <- PG.execute_ conn $ fromString cmd
    forever $ do
      -- Handle notifications
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel == T.encodeUtf8 (T.pack notifyChannel) -> do -- Notification is on notifyChannel
          case decodeValue' $ LBS.fromStrict message of
            Just a -> atomically $ writeTChan nChan a
            _ -> putStrLn $ "listenDB: Could not parse message on updates channel: " <> show message
        _ -> putStrLn $ "listenDB: Received a message on unexpected channel: " <> show channel
  return (nChan, killThread daemonThread)

-- | Starts a thread that listens for updates to the db and returns a
-- 'DbNotification' retrieval function and finalizer
startNotificationListener :: FromJSON notifyMessage => Pool PG.Connection -> IO (IO notifyMessage, IO ())
startNotificationListener pool = do
  (chan, nkill) <- notificationListener pool
  chan' <- atomically $ dupTChan chan
  return (atomically $ readTChan chan', nkill)

getSchemaName :: (PostgresRaw m, Monad m)
              => m String
getSchemaName =  do
  searchPath <- getSearchPath
  let searchPathComponents = wordsBy (==',') searchPath
      schemaName = case searchPathComponents of
       (x:_:_:_) -> x
       _ -> "public"
  return schemaName

data DbNotification n = DbNotification
  { _dbNotification_schemaName :: SchemaName
  , _dbNotification_notificationType :: NotificationType
  , _dbNotification_message :: DSum n Identity
  } deriving (Generic)

deriving instance (Show (DSum n Identity)) => Show (DbNotification n)

instance
  ( Has' ToJSON n Identity
  , ForallF ToJSON n
  )
  => ToJSON (DbNotification n)

instance
  ( Has' FromJSON n Identity
  , FromJSON (Some n)
  ) => FromJSON (DbNotification n)

notify ::
  ( PostgresRaw m
  , Monad m
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  )
  => NotificationType
  -> n a
  -> a
  -> m ()
notify nt n a = do
  schemaName <- getSchemaName
  let cmd = "NOTIFY " <> fromString notifyChannel <> ", ?"
      notifyMsg = DbNotification
        { _dbNotification_schemaName = SchemaName $ T.pack schemaName
        , _dbNotification_notificationType = nt
        , _dbNotification_message = n :=> Identity a
        }
  _ <- execute cmd
    [ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ encode notifyMsg
    ]
  return ()

-- Class for relating application-specific db notification types with db entity types.
-- class (HasId a) => HasNotification n a | a -> n where
--   notification :: proxy a -> n (Id a)
