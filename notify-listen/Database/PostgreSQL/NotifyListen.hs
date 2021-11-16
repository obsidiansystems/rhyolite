{-|
Description:
  Use the postgres NOTIFY/LISTEN commands to distribute information about
  updates to the database.
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}

module Database.PostgreSQL.NotifyListen where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TChan)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity (Identity(..))
import Data.List.Split (wordsBy)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, withResource)
import Data.Some (Some)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Notification as PG
import GHC.Generics (Generic)

-- | Wrapper for the name of the channel over which notifications are sent
newtype NotificationChannel
  = NotificationChannel { unNotificationChannel :: String }

-- | The channel name (@"updates"@) used for insert/update/delete
-- notifications.
defaultNotificationChannel :: NotificationChannel
defaultNotificationChannel = NotificationChannel "updates"

-- | Constructs a @LISTEN@ command with the given channel name
listenCmd :: NotificationChannel -> PG.Query
listenCmd (NotificationChannel chan) = fromString $ "LISTEN " <> chan

-- | Starts a thread to receive messages about changes to the database via the
-- postgres @LISTEN@ mechanism.
notificationListener
  :: (FromJSON notifyMessage)
  => NotificationChannel
  -- ^ The channel to listen on
  -> Pool PG.Connection
  -- ^ Connection pool
  -> IO (TChan notifyMessage, IO ())
  -- ^ @notifyMessage@ is usually a 'DbNotification'
notificationListener notifyChannel db = do
  nChan <- STM.newBroadcastTChanIO
  daemonThread <- forkIO $ withResource db $ \conn -> do
    let cmd = listenCmd notifyChannel
    _ <- PG.execute_ conn cmd
    forever $ do
      -- Handle notifications
      PG.Notification _ channel message <- PG.getNotification conn
      case channel of
        _ | channel ==  channelToByteString notifyChannel -> do
          -- Notification is on the expected NOTIFY channel
          case decode $ LBS.fromStrict message of
            Just a -> STM.atomically $ STM.writeTChan nChan a
            _ -> putStrLn $ errorMessage notifyChannel $
              "Could not parse message: " <> show message
        _ -> putStrLn $ errorMessage notifyChannel $
          "Received a message on unexpected channel: " <> show channel
  return (nChan, killThread daemonThread)
  where
    channelToByteString :: NotificationChannel -> BS.ByteString
    channelToByteString = T.encodeUtf8 . T.pack . unNotificationChannel
    errorMessage :: NotificationChannel -> String -> String
    errorMessage (NotificationChannel c) err =
      "notificationListener: channel \"" <> c <> "\": " <> err

-- | Starts a thread that listens for updates to the db and returns a
-- 'DbNotification' retrieval function and finalizer
startNotificationListener
  :: FromJSON notifyMessage
  => NotificationChannel
  -> Pool PG.Connection
  -> IO (IO notifyMessage, IO ())
startNotificationListener notifyChannel pool = do
  (chan, nkill) <- notificationListener notifyChannel pool
  chan' <- STM.atomically $ STM.dupTChan chan
  return (STM.atomically $ STM.readTChan chan', nkill)

-- | Get the schema name out of the current @search_path@
getSchemaName :: PG.Connection
              -> IO String
getSchemaName conn =  do
  searchPath <- getSearchPath conn
  let searchPathComponents = wordsBy (==',') searchPath
      schemaName = case searchPathComponents of
       (x:_:_:_) -> x
       _ -> "public"
  return schemaName

-- | Get the current @search_path@
getSearchPath :: PG.Connection -> IO String
getSearchPath conn = do
  rows <- PG.query_ conn "SHOW search_path"
  case listToMaybe rows of
    Nothing -> error "getSearchPath: Unexpected result from queryRaw"
    Just (PG.Only searchPath) -> return searchPath

-- | Used to indicate whether a given notification is an insert, update, or
-- deletion. Deletions, for instance, may require special handling, as the
-- deleted data will no longer be in the database.
data NotificationType
  = NotificationType_Insert
  | NotificationType_Update
  | NotificationType_Delete
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON NotificationType
instance FromJSON NotificationType

-- | A notification payload.
data DbNotification n = DbNotification
  { _dbNotification_schemaName :: SchemaName
  -- ^ The schema that was updated
  , _dbNotification_notificationType :: NotificationType
  -- ^ Whether this is an insert, update, or delete
  , _dbNotification_message :: DSum n Identity
  -- ^ The notification message itself. The notification protocol is defined as
  -- a GADT. See 'notify'.
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

-- | Constructs a @NOTIFY@ command with the given channel name
notifyCmd :: NotificationChannel -> PG.Query
notifyCmd (NotificationChannel chan) = fromString $ "NOTIFY " <> chan <> ", ?"

-- | Sends a notification over a given channel. Notification payloads must have
-- a JSON representation for transmission.
-- 
-- The notification message itself is wrapped in a 'DSum' to allow messages of
-- different types. To use this, construct a GADT that defines your
-- notification protocol:
--
-- > data Notice a where
-- >   Notify_Account :: Notify (Id Account)
-- >   Notify_Chatroom :: Notify (Id Chatroom)
-- >   Notify_Message :: Notify (Id Message)
--
-- Now, you can use the GADT constructor that corresponds to the particular
-- notification payload type you'd like to send. For example, here's an
-- invocation that sends a notification that a new account has been created:
--
-- > notify myChan conn NotificationType_Insert Notify_Account accountId
--
-- NB: The maximum payload size is 8000 bytes (this is currently neither
-- checked nor enforced in this library):
--
-- > The "payload" string to be communicated along with the notification. This
-- > must be specified as a simple string literal. In the default configuration
-- > it must be shorter than 8000 bytes. (If binary data or large amounts of
-- > information need to be communicated, it's best to put it in a database table
-- > and send the key of the record.)
-- (Source: https://www.postgresql.org/docs/9.0/sql-notify.html)
notify ::
  ( Has' ToJSON notice Identity
  , ForallF ToJSON notice
  )
  => NotificationChannel
  -> PG.Connection
  -> NotificationType
  -> notice a
  -> a
  -> IO ()
notify notifyChannel conn nt n a = do
  schemaName <- getSchemaName conn
  let cmd = notifyCmd notifyChannel
      notifyMsg = DbNotification
        { _dbNotification_schemaName = SchemaName $ T.pack schemaName
        , _dbNotification_notificationType = nt
        , _dbNotification_message = n :=> Identity a
        }
  _ <- PG.execute conn cmd
    [ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ encode notifyMsg
    ]
  return ()

-- | Wrapper for database schema names
newtype SchemaName = SchemaName { unSchemaName :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, Typeable, Generic)
