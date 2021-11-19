{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# options_ghc -fno-warn-orphans #-}

module Rhyolite.DB.Groundhog.Serializable
  ( Serializable
  , runSerializable
  , toDbPersist
  , unsafeLiftDbPersist
  , unsafeMkSerializable
  , unSerializable
  ) where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader (withReaderT)
import Data.Coerce (coerce)
import qualified Database.Groundhog.Core as Hog
import qualified Database.Groundhog.Generic.Migration as Mig
import Database.Groundhog.Postgresql (Postgresql(..))
import Database.PostgreSQL.Serializable
import Database.PostgreSQL.Simple.Groundhog ()

import qualified Control.Monad.State as S

instance Hog.PersistBackend Serializable where
  type PhantomDb Serializable = Hog.PhantomDb (Hog.DbPersist Postgresql (LoggingT IO))
  type TableAnalysis Serializable = Hog.TableAnalysis (Hog.DbPersist Postgresql (LoggingT IO))
  insert = unsafeLiftDbPersist . Hog.insert
  insert_ = unsafeLiftDbPersist . Hog.insert_
  insertBy u v = unsafeLiftDbPersist $ Hog.insertBy u v
  insertByAll = unsafeLiftDbPersist . Hog.insertByAll
  replace k v = unsafeLiftDbPersist $ Hog.replace k v
  replaceBy u v = unsafeLiftDbPersist $ Hog.replaceBy u v
  select = unsafeLiftDbPersist . Hog.select
  selectAll = unsafeLiftDbPersist Hog.selectAll
  get = unsafeLiftDbPersist . Hog.get
  getBy = unsafeLiftDbPersist . Hog.getBy
  update us c = unsafeLiftDbPersist $ Hog.update us c
  delete = unsafeLiftDbPersist . Hog.delete
  deleteBy = unsafeLiftDbPersist . Hog.deleteBy
  deleteAll = unsafeLiftDbPersist . Hog.deleteAll
  count = unsafeLiftDbPersist . Hog.count
  countAll = unsafeLiftDbPersist . Hog.countAll
  project p o = unsafeLiftDbPersist $ Hog.project p o
  migrate i v = S.mapStateT unsafeLiftDbPersist $ Hog.migrate i v
  executeRaw c q p = unsafeLiftDbPersist $ Hog.executeRaw c q p
  queryRaw c q ps f =
    unsafeLiftDbPersist $ Hog.queryRaw c q ps $ \rp -> toDbPersist (f $ unsafeLiftDbPersist rp)
  insertList = unsafeLiftDbPersist . Hog.insertList
  getList = unsafeLiftDbPersist . Hog.getList

instance Mig.SchemaAnalyzer Serializable where
  schemaExists = unsafeLiftDbPersist . Mig.schemaExists
  getCurrentSchema = unsafeLiftDbPersist Mig.getCurrentSchema
  listTables = unsafeLiftDbPersist . Mig.listTables
  listTableTriggers = unsafeLiftDbPersist . Mig.listTableTriggers
  getTableAnalysis = unsafeLiftDbPersist Mig.getTableAnalysis
  analyzeTable info = unsafeLiftDbPersist . Mig.analyzeTable info
  analyzeTrigger = unsafeLiftDbPersist . Mig.analyzeTrigger
  analyzeFunction = unsafeLiftDbPersist . Mig.analyzeFunction
  getMigrationPack i = unsafeHoistCoerceFromReaderT . coerce <$> unsafeLiftDbPersist (Mig.getMigrationPack i)

toDbPersist :: forall a. Serializable a -> Hog.DbPersist Postgresql (LoggingT IO) a
toDbPersist act = Hog.DbPersist $ withReaderT coerce (unSerializable act)

unsafeLiftDbPersist :: forall a. Hog.DbPersist Postgresql (LoggingT IO) a -> Serializable a
unsafeLiftDbPersist (Hog.DbPersist act) = unsafeMkSerializable $ withReaderT coerce act
