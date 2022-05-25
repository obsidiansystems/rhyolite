{-|
Description: Notifications in groundhog

Legacy adapter for postgres NOTIFY/LISTEN functionality in groundhog-based
rhyolite projects.
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Rhyolite.DB.NotifyListen.Groundhog
  ( module Rhyolite.DB.NotifyListen.Groundhog
  , NL.NotificationType (..)
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Coerce
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Identity
import Data.Pool
import Data.These
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Instances ()
import Database.Groundhog.Postgresql
import Database.Id.Class (HasId, Id, IdData)
import Database.Id.Groundhog (DefaultKeyId, fromId, toId)
import qualified Rhyolite.DB.NotifyListen as NL
import Rhyolite.DB.NotifyListen (NotificationType(..))
import Rhyolite.Aeson.Orphans ()
import Database.PostgreSQL.Simple.Class
import GHC.Generics

notificationListener
  :: (FromJSON notifyMessage)
  => Pool Postgresql
  -> IO (TChan notifyMessage, IO ())
notificationListener = NL.notificationListener . coerce

startNotificationListener
  :: FromJSON notifyMessage
  => Pool Postgresql
  -> IO (IO notifyMessage, IO ())
startNotificationListener =
  NL.startNotificationListener . coerce

notify
  :: ( Has' ToJSON n Identity
     , ForallF ToJSON n
     , Psql m
     )
  => NL.NotificationType
  -> n a
  -> a
  -> m ()
notify nt n a = NL.notify nt n a

-- | Class for relating application-specific db notification types with db
-- entity types.
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
     , Psql m
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
     , Psql m
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
  , Psql m
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
  , Psql m
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
  , Psql m
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
  , Psql m
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
     , Psql m
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
     , Psql m
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
     , Psql m
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
     , Psql m
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
  , Psql m
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
