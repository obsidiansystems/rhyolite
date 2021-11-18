{-|
Description:
  Notifications about database changes in beam
-}
{-# Language ConstraintKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FunctionalDependencies #-}
{-# Language GADTs #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}
module Rhyolite.DB.NotifyListen.Beam
  ( HasNotification(..)
  , insertAndNotify
  , updateAndNotify
  , deleteAndNotify
  , HasChangeNotification(..)
  , Change(..)
  , changeOld
  , changeNew
  , insertAndNotifyChange
  , updateAndNotifyChange
  , deleteAndNotifyChange
  ) where

import Data.Aeson
import Data.Constraint.Extras
import Data.Constraint.Forall
import Data.Functor.Identity
import Data.These
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.Class
import GHC.Generics
import Rhyolite.DB.NotifyListen

-- | Type synonym for things that can be used in a postgres query equality
-- comparison. If @x@ satisfies these constraints, you should be able to use
-- @x@ in a beam query like this:
--
-- > (\t -> t ==. val_ x)
--
type HasSqlEquality t =
  ( Generic (t (HasConstraint (HasSqlEqualityCheck Postgres)))
  , Generic (t Identity)
  , Generic (t Exposed)
  , (Generic (t (HasConstraint (HasSqlValueSyntax PgValueSyntax))))
  , (GFieldsFulfillConstraint
      (HasSqlValueSyntax PgValueSyntax)
      (Rep (t Exposed))
      (Rep (t (HasConstraint (HasSqlValueSyntax PgValueSyntax)))))
  , (GFieldsFulfillConstraint
      (HasSqlEqualityCheck Postgres)
      (Rep (t Exposed))
      (Rep (t (HasConstraint (HasSqlEqualityCheck Postgres)))))
  )

-- | Class for relating application-specific db notification types with db
-- table entities. The example below shows how this can be used. Here's a
-- table definition:
--
-- > data Person f = Person { personName :: Columnar f Text }
-- > instance Table Person where
-- >   data PrimaryKey Person f = PersonId (Columnar f Text)
-- >     deriving (Generic, Beamable)
-- >   primaryKey = PersonId . personName
--
-- You can use a GADT to define the types of notifications you expect to
-- receive. Below, we're saying that we expect a notification payload carrying
-- the primary key of the changed row.
--
-- > data DbNotify a where
-- >   DbNotify_Person :: DbNotify (PrimaryKey Person Identity)
--
-- We can now implement as 'HasNotification' instance:
--
-- > instance HasNotification DbNotify Person where
-- >   notification _ = DbNotify_Person
--
-- Now, we can call 'insertAndNotify', 'updateAndNotify', or 'deleteAndNotify'
-- to modify the @Person@ table, and this instance will be used to produce the
-- right type of notification.
--
class HasNotification n a | a -> n where
  notification :: DatabaseEntity be db (TableEntity a) -> n (PrimaryKey a Identity)

-- | Insert a value into a table and send a db notification with information
-- about the change.
insertAndNotify :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeamInsertReturning be m
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table into which the new row will be inserted
  -> (forall s. t (QExpr be s))
  -- ^ The value to be inserted
  -> m (Maybe (PrimaryKey t Identity))
  -- ^ The primary key of the new row, if the insert was successful
insertAndNotify tbl g = do
  xs <- runInsertReturningList $ insert tbl $ insertExpressions [g]
  case xs of
    [x] -> do
      let xid = primaryKey x
      notify NotificationType_Insert (notification tbl) xid
      pure $ Just $ primaryKey x
    _ -> pure Nothing

-- | Update a row in a table and send a db notification with information about
-- the change. This function only updates the row specified by the primary key
-- argument.
updateAndNotify :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeamUpdateReturning be m
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  , HasSqlEquality (PrimaryKey t)
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table being updated
  -> PrimaryKey t Identity
  -- ^ The primary key of the row to update
  -> (forall s. t (QField s) -> QAssignment be s)
  -- ^ The actual update to perform
  -- (e.g., @(\c -> addressCountry (customerAddress c) <-. val_ (Just "USA"))@)
  -> m (Maybe (PrimaryKey t Identity))
  -- ^ The primary key of the updated row, if the update was successful
updateAndNotify tbl k g = do
  xs <- runUpdateReturningList $
    update tbl g (\t -> primaryKey t ==. val_ k)
  case xs of
    [x] -> do
      let xid = primaryKey x
      notify NotificationType_Update (notification tbl) xid
      pure $ Just $ primaryKey x
    _ -> pure Nothing

-- | Delete a row in a table and send a db notification with information about
-- the change. This function only deletes the row specified by the primary key
-- argument.
deleteAndNotify :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeam be m
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  , HasSqlEquality (PrimaryKey t)
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table to delete from
  -> PrimaryKey t Identity
  -- ^ The primary key of the row to delete
  -> m (PrimaryKey t Identity)
  -- ^ The primary key that was deleted
deleteAndNotify tbl k = do
  runDelete $ delete tbl (\t -> primaryKey t ==. val_ k)
  notify NotificationType_Delete (notification tbl) k
  pure k

-- | Similar to 'HasNotification' except that this class provides more detailed
-- information about what changed, including the old and new values, where
-- applicable.
class HasChangeNotification n a | a -> n where
  changeNotification :: DatabaseEntity be db (TableEntity a) -> n (Change a)

-- | Information about a database change, including both the old and new
-- values, where they exist.
data Change a = Change
  { _change_id :: PrimaryKey a Identity
  , _change_oldNew :: These (a Identity) (a Identity)
  -- ^ @These oldValue newValue@
  } deriving (Generic)

-- | Accessor for the old value, if it exists.
changeOld :: Change a -> Maybe (a Identity)
changeOld = these Just (const Nothing) (\old _ -> Just old) . _change_oldNew

-- | Accessor for the new value, if it exists.
changeNew :: Change a -> Maybe (a Identity)
changeNew = these (const Nothing) Just (\_ new -> Just new) . _change_oldNew

deriving instance (Eq (PrimaryKey a Identity), Eq (a Identity))
  => Eq (Change a)
deriving instance (Show (PrimaryKey a Identity), Show (a Identity))
  => Show (Change a)
instance (ToJSON (PrimaryKey a Identity), ToJSON (a Identity))
  => ToJSON (Change a)
instance (FromJSON (PrimaryKey a Identity), FromJSON (a Identity))
  => FromJSON (Change a)

-- | Insert a row into the database and send a 'Change' notification.
insertAndNotifyChange :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeamInsertReturning be m
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasChangeNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table into which the new row will be inserted
  -> (forall s. t (QExpr be s))
  -- ^ The value to be inserted
  -> m (Maybe (PrimaryKey t Identity))
  -- ^ The primary key of the new row, if the insert was successful
insertAndNotifyChange tbl g = do
  xs <- runInsertReturningList $ insert tbl $ insertExpressions [g]
  case xs of
    [x] -> do
      let xid = primaryKey x
          change = Change { _change_id = xid, _change_oldNew = That x }
      notify NotificationType_Insert (changeNotification tbl) change
      pure $ Just xid
    _ -> pure Nothing

-- | Update a row in the database and send a 'Change' notification about the
-- modification. Note that this function retrieves the entire row before the
-- modification and sends a notification including both the old and new values.
-- See the size limitation note on 'Rhyolite.DB.NotifyListen.notify'.
updateAndNotifyChange :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeamUpdateReturning be m
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasChangeNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  , HasSqlEquality (PrimaryKey t)
  , Database be db
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table being updated
  -> PrimaryKey t Identity
  -- ^ The primary key of the row to update
  -> (forall s. t (QField s) -> QAssignment be s)
  -- ^ The actual update to perform
  -- (e.g., @(\c -> addressCountry (customerAddress c) <-. val_ (Just "USA"))@)
  -> m (Maybe (PrimaryKey t Identity))
  -- ^ The primary key of the updated row, if the update was successful
updateAndNotifyChange tbl k g = do
  mx <- selectByKey tbl k
  case mx of
    Nothing -> pure Nothing
    Just old -> do
      xs <- runUpdateReturningList $
        update tbl g (\t -> primaryKey t ==. val_ k)
      case xs of
        [new] -> do
          let change = Change
                { _change_id = k
                , _change_oldNew = These old new
                }
          notify NotificationType_Update (changeNotification tbl) change
          pure $ Just k
        _ -> pure Nothing

-- | Delete a row from the database and send a 'Change' notification. Note that
-- this retrieves the row before deleting it.
deleteAndNotifyChange :: forall be db t n m.
  ( be ~ Postgres
  , MonadBeam be m
  , Database Postgres db
  , BeamSqlBackend be
  , Table t
  , Has' ToJSON n Identity
  , ForallF ToJSON n
  , HasChangeNotification n t
  , Psql m
  , FromBackendRow be (t Identity)
  , HasSqlEquality (PrimaryKey t)
  )
  => DatabaseEntity be db (TableEntity t)
  -- ^ The table to delete from
  -> PrimaryKey t Identity
  -- ^ The primary key of the row to delete
  -> m (Maybe (PrimaryKey t Identity))
  -- ^ The primary key of the deleted row, if the row existed when we tried
deleteAndNotifyChange tbl k = do
  mv <- selectByKey tbl k
  case mv of
    Nothing -> pure Nothing
    Just v -> do
      let change = Change { _change_id = k, _change_oldNew = This v }
      runDelete $ delete tbl (\t -> primaryKey t ==. val_ k)
      notify NotificationType_Delete (changeNotification tbl) change
      pure $ Just k

selectByKey ::
  ( be ~ Postgres
  , Database Postgres db
  , MonadBeam be m
  , BeamSqlBackend be
  , FromBackendRow be (t Identity)
  , HasQBuilder be
  , Beamable t
  , Table t
  , HasSqlEquality (PrimaryKey t)
  )
  => DatabaseEntity be db (TableEntity t)
  -> PrimaryKey t Identity
  -> m (Maybe (t Identity))
selectByKey tbl k = runSelectReturningOne $
    select $ filter_ (\t -> primaryKey t ==. val_ k) (all_ tbl)
