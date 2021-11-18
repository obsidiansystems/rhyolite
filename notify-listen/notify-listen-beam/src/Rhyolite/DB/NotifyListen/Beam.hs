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

class HasNotification n a | a -> n where
  notification :: DatabaseEntity be db (TableEntity a) -> n (PrimaryKey a Identity)

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
  -> (forall s. t (QExpr be s))
  -> m (Maybe (PrimaryKey t Identity))
insertAndNotify tbl g = do
  xs <- runInsertReturningList $ insert tbl $ insertExpressions [g]
  case xs of
    [x] -> do
      let xid = primaryKey x
      notify NotificationType_Insert (notification tbl) xid
      pure $ Just $ primaryKey x
    _ -> pure Nothing

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
  -> PrimaryKey t Identity
  -> (forall s. t (QField s) -> QAssignment be s)
  -> m (Maybe (PrimaryKey t Identity))
updateAndNotify tbl k g = do
  xs <- runUpdateReturningList $
    update tbl g (\t -> primaryKey t ==. val_ k)
  case xs of
    [x] -> do
      let xid = primaryKey x
      notify NotificationType_Update (notification tbl) xid
      pure $ Just $ primaryKey x
    _ -> pure Nothing

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
  -> PrimaryKey t Identity
  -> m (PrimaryKey t Identity)
deleteAndNotify tbl k = do
  runDelete $ delete tbl (\t -> primaryKey t ==. val_ k)
  notify NotificationType_Delete (notification tbl) k
  pure k

class HasChangeNotification n a | a -> n where
  changeNotification :: DatabaseEntity be db (TableEntity a) -> n (Change a)

data Change a = Change
  { _change_id :: PrimaryKey a Identity
  , _change_oldNew :: These (a Identity) (a Identity)
  } deriving (Generic)

changeOld :: Change a -> Maybe (a Identity)
changeOld = these Just (const Nothing) (\old _ -> Just old) . _change_oldNew

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
  -> (forall s. t (QExpr be s))
  -> m (Maybe (PrimaryKey t Identity))
insertAndNotifyChange tbl g = do
  xs <- runInsertReturningList $ insert tbl $ insertExpressions [g]
  case xs of
    [x] -> do
      let xid = primaryKey x
          change = Change { _change_id = xid, _change_oldNew = That x }
      notify NotificationType_Insert (changeNotification tbl) change
      pure $ Just xid
    _ -> pure Nothing

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
  -> PrimaryKey t Identity
  -> (forall s. t (QField s) -> QAssignment be s)
  -> m (Maybe (PrimaryKey t Identity))
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
  -> PrimaryKey t Identity
  -> m (Maybe (PrimaryKey t Identity))
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
