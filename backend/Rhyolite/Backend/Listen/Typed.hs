{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Rhyolite.Backend.Listen.Typed
  ( DbNotification(..)
  , notify
  , NotificationType(..)
  , HasNotification(..)
  , insertAndNotifyWith
  , insertAndNotify
  , insertByAndNotifyWith
  , insertByAndNotify
  , insertByAllAndNotifyWith
  , insertByAllAndNotify
  , updateAndNotify
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
import Database.Groundhog.Expression

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