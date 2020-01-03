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

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Monad (forever, void, forM_)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity (..))
import Data.List.Split (wordsBy)
import Data.Pool (Pool, withResource)
import Data.Semigroup ((<>))
import Data.Some (Some)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.These
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import Database.Id.Class (HasId, Id, IdData)
import Database.Id.Groundhog (fromId, toId, DefaultKeyId)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import GHC.Generics (Generic)

import Rhyolite.Backend.DB (getSearchPath)
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
  => Pool Postgresql
  -> IO (TChan notifyMessage, IO ()) -- "notifyMessage" is usually a DbNotification
notificationListener db = do
  nChan <- newBroadcastTChanIO
  daemonThread <- forkIO $ withResource db $ \(Postgresql conn) -> do
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
startNotificationListener :: FromJSON notifyMessage => Pool Postgresql -> IO (IO notifyMessage, IO ())
startNotificationListener pool = do
  (chan, nkill) <- notificationListener pool
  chan' <- atomically $ dupTChan chan
  return (atomically $ readTChan chan', nkill)

getSchemaName :: PersistBackend m
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

notify
  :: ( Has' ToJSON n Identity
     , PersistBackend m
     , ForallF ToJSON n
     )
  => NotificationType
  -> n a
  -> a
  -> m ()
notify nt n a = do
  schemaName <- getSchemaName
  let cmd = "NOTIFY " <> notifyChannel <> ", ?"
      notifyMsg = DbNotification
        { _dbNotification_schemaName = SchemaName $ T.pack schemaName
        , _dbNotification_notificationType = nt
        , _dbNotification_message = n :=> Identity a
        }
  void $ executeRaw False cmd
    [ PersistString $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ encode notifyMsg
    ]

-- Class for relating application-specific db notification types with db entity types.
class (HasId a) => HasNotification n a | a -> n where
  notification :: proxy a -> n (Id a)

insertAndNotifyWith
  :: ( HasNotification n a
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , PersistBackend m
     , DefaultKeyId a
     , PersistEntity a
     , ToJSON (IdData a)
     )
  => (AutoKey a -> Id a) -> a -> m (Id a)
insertAndNotifyWith f a = do
  autokey <- insert a
  let aid = f autokey
  notify NotificationType_Insert (notification aid) aid
  return aid

insertAndNotify
  :: ( HasNotification n a
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , PersistBackend m
     , DefaultKeyId a
     , PersistEntity a
     , ToJSON (IdData a)
     , DefaultKey a ~ AutoKey a
     )
  => a -> m (Id a)
insertAndNotify = insertAndNotifyWith toId

insertByAndNotifyWith ::
  ( HasNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , ToJSON (IdData a)
  , IsUniqueKey (Key a (Unique u))
  )
  => (AutoKey a -> Id a)
  -> u (UniqueMarker a)
  -> a
  -> m (Maybe (Id a))
insertByAndNotifyWith f u t = do
  eak <- insertBy u t
  case eak of
    Left _ -> return Nothing
    Right ak -> do
      let tid = f ak
      notify NotificationType_Insert (notification tid) tid
      return (Just tid)

insertByAndNotify ::
  ( HasNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , ToJSON (IdData a)
  , IsUniqueKey (Key a (Unique u))
  , AutoKey a ~ DefaultKey a
  )
  => u (UniqueMarker a)
  -> a
  -> m (Maybe (Id a))
insertByAndNotify u t = insertByAndNotifyWith toId u t

insertByAllAndNotifyWith ::
  ( HasNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , ToJSON (IdData a)
  )
  => (AutoKey a -> Id a)
  -> a
  -> m (Maybe (Id a))
insertByAllAndNotifyWith f t = do
  eak <- insertByAll t
  case eak of
    Left _ -> return Nothing
    Right ak -> do
      let tid = f ak
      notify NotificationType_Insert (notification tid) tid
      return (Just tid)

insertByAllAndNotify ::
  ( HasNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , ToJSON (IdData a)
  , AutoKey a ~ DefaultKey a
  )
  => a
  -> m (Maybe (Id a))
insertByAllAndNotify t = insertByAllAndNotifyWith toId t

updateAndNotify :: forall a v c m n.
     ( HasNotification n a
     , Has' ToJSON n Identity
     , ToJSON (IdData a)
     , Expression (PhantomDb m) (RestrictionHolder v c) (DefaultKey a)
     , PersistEntity v
     , PersistEntity a
     , PersistBackend m
     , Unifiable (AutoKeyField v c) (DefaultKey a)
     , DefaultKeyId a
     , _
     )
  => Id a
  -> [Update (PhantomDb m) (RestrictionHolder v c)]
  -> m ()
updateAndNotify tid dt = do
  update dt (AutoKeyField ==. fromId tid)
  notify NotificationType_Update (notification tid :: n (Id a)) tid

deleteAndNotify :: forall n a m.
  ( HasNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , ToJSON (IdData a)
  , _
  )
  => Id a -> m ()
deleteAndNotify aid = do
  deleteBy (fromId aid :: Key a BackendSpecific)
  notify NotificationType_Delete (notification aid) aid

data Change a = Change
  { _change_id :: Id a
  , _change_oldNew :: These a a
  } deriving (Generic)

changeOld :: Change a -> Maybe a
changeOld = these Just (const Nothing) (\old _ -> Just old) . _change_oldNew

changeNew :: Change a -> Maybe a
changeNew = these (const Nothing) Just (\_ new -> Just new) . _change_oldNew

deriving instance (Eq (IdData a), Eq a) => Eq (Change a)
deriving instance (Show (IdData a), Show a) => Show (Change a)
instance (ToJSON (IdData a), ToJSON a) => ToJSON (Change a)
instance (FromJSON (IdData a), FromJSON a) => FromJSON (Change a)

insertAndNotifyChangeWith
  :: ( HasChangeNotification n a
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , PersistBackend m
     , DefaultKeyId a
     , PersistEntity a
     , ToJSON (IdData a)
     )
  => (AutoKey a -> Id a) -> a -> m (Id a)
insertAndNotifyChangeWith f a = do
  autokey <- insert a
  let aid = f autokey
      change = Change
        { _change_id = aid
        , _change_oldNew = That a
        }
  notify NotificationType_Insert (changeNotification change) change
  return aid

insertAndNotifyChange
  :: ( HasChangeNotification n a
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , PersistBackend m
     , DefaultKeyId a
     , PersistEntity a
     , ToJSON (IdData a)
     , DefaultKey a ~ AutoKey a
     )
  => a -> m (Id a)
insertAndNotifyChange = insertAndNotifyChangeWith toId

updateAndNotifyChange ::
     ( HasChangeNotification n a
     , Has' ToJSON n Identity
     , ForallF ToJSON n
     , ToJSON (IdData a)
     , PersistEntity a
     , PrimitivePersistField (Key a BackendSpecific)
     , PersistBackend m
     , DefaultKeyId a
     , DefaultKey a ~ Key a BackendSpecific
     )
  => Id a
  -> (a -> a)
  -> m ()
updateAndNotifyChange tid f = do
  mv <- get (fromId tid)
  forM_ mv $ \oldV -> do
    let newV = f oldV
        change = Change
          { _change_id = tid
          , _change_oldNew = These oldV newV
          }
    replace (fromId tid) newV
    notify NotificationType_Update (changeNotification change) change

deleteAndNotifyChange :: forall n a m.
  ( HasChangeNotification n a
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , PersistBackend m
  , DefaultKeyId a
  , PersistEntity a
  , PrimitivePersistField (Key a BackendSpecific)
  , ToJSON (IdData a)
  , DefaultKey a ~ Key a BackendSpecific
  )
  => Id a -> m ()
deleteAndNotifyChange aid = do
  mv <- get (fromId aid)
  deleteBy (fromId aid :: Key a BackendSpecific)
  forM_ mv $ \v -> do
    let change = Change
          { _change_id = aid
          , _change_oldNew = This v
          }
    notify NotificationType_Delete (changeNotification change) change

class (HasId a) => HasChangeNotification n a | a -> n where
  changeNotification :: proxy a -> n (Change a)

