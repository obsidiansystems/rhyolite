{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Rhyolite.Backend.Listen.Typed
  ( DbNotification(..)
  , notify
  , insertAndNotifyWith
  , NotificationType(..)
  )
  where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Sum.Orphans ()
import Data.Functor.Identity
import Data.Some (Some)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Rhyolite.Backend.Listen (NotificationType(..), getSchemaName, notifyChannel)
import Rhyolite.Schema
import Rhyolite.Backend.Schema
import Rhyolite.Backend.Schema.Class (DefaultKeyId)

import Database.Groundhog.Core

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
      notification = DbNotification
        { _dbNotification_schemaName = SchemaName $ T.pack schemaName
        , _dbNotification_notificationType = nt
        , _dbNotification_message = n :=> Identity a
        }
  void $ executeRaw False cmd
    [ PersistString $ T.unpack $ T.decodeUtf8 $ LBS.toStrict $ encode notification
    ]


insertAndNotifyWith
  :: ( PersistBackend m
     , DefaultKey a ~ AutoKey a
     , DefaultKeyId a
     , PersistEntity a
     , ToJSON (IdData a)
     , Has' ToJSON n Identity
     , ForallF ToJSON n

     )
  => n (Id a)
  -> a
  -> m (Id a)
insertAndNotifyWith n a = do
  aid <- toId <$> insert a
  notify NotificationType_Insert n aid
  return aid
