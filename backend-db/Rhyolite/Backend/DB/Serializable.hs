{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Backend.DB.Serializable
  ( Serializable
  , SqlSerializationError (..)
  , runSerializable
  , toDbPersist
  , unsafeLiftDbPersist
  , unsafeMkSerializable
  , unSerializable
  , withSqlSerializationErrorWrapping
  ) where

import qualified Control.Exception as E
import Control.Monad.Base (MonadBase (liftBase))
import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.Catch as MonadCatch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Logger (MonadLogger, LoggingT)
import qualified Control.Monad.State as S
import Data.Coerce (coerce)
import qualified Database.Groundhog.Generic.Migration as Mig
import Database.Groundhog.Postgresql (Postgresql (..))
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Data.Pool (Pool, withResource)
import qualified Database.Groundhog.Core as Hog

import qualified Rhyolite.Backend.DB.PsqlSimple as PsqlSimple
import Rhyolite.Logging (LoggingEnv, runLoggingEnv)

-- | A monad for database transactions with serializable isolation level.
--
-- Because this monad may retry execution of code automatically, it does not lawfully lift any effects other
-- than Reader, which it commutes with, so there is no gain in having it be a transformer.
--
-- It "disallows" (makes harder) arbitrary IO.
-- It "disallows" (makes harder) catching IO exceptions *inside* the transaction.
newtype Serializable a = Serializable (ReaderT Pg.Connection (LoggingT IO) a)
  deriving (Functor, Applicative, Monad, MonadThrow, MonadLogger)
  -- NOTE: We *intentionally* leave out
  --   - 'MonadCatch' so you can't accidentally mask a serialization error from the outer retry logic.
  --   - 'MonadBaseControl' (et al) for the same reason.
  --   - 'MonadIO' so you can't execute arbitrary IO.

instance MonadBase Serializable Serializable where
  liftBase = id

instance PsqlSimple.PostgresRaw Serializable where
  execute psql qs = unsafeLiftDbPersist $ PsqlSimple.execute psql qs
  execute_ = unsafeLiftDbPersist . PsqlSimple.execute_
  executeMany psql qs = unsafeLiftDbPersist $ PsqlSimple.executeMany psql qs
  query psql qs = unsafeLiftDbPersist $ PsqlSimple.query psql qs
  query_ = unsafeLiftDbPersist . PsqlSimple.query_
  queryWith parser psql qs = unsafeLiftDbPersist $ PsqlSimple.queryWith parser psql qs
  queryWith_ parser psql = unsafeLiftDbPersist $ PsqlSimple.queryWith_ parser psql
  formatQuery psql qs = unsafeLiftDbPersist $ PsqlSimple.formatQuery psql qs
  returning psql qs = unsafeLiftDbPersist $ PsqlSimple.returning psql qs

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
  getMigrationPack i = coerce <$> unsafeLiftDbPersist (Mig.getMigrationPack i)


data SqlSerializationError = SqlSerializationError deriving (Eq, Ord, Show)
instance E.Exception SqlSerializationError

withSqlSerializationErrorWrapping :: forall m a. (MonadCatch.MonadCatch m, MonadThrow m) => m a -> m a
withSqlSerializationErrorWrapping = flip MonadCatch.catches
  [ MonadCatch.Handler $ \(e :: Pg.SqlError) -> convert id e
  , MonadCatch.Handler $ \(e :: PsqlSimple.WrappedSqlError) -> convert PsqlSimple._wrappedSqlError_error e
  ]
  where
    convert :: E.Exception e => (e -> Pg.SqlError) -> e -> m a
    convert toSqlError e = if Pg.isSerializationError (toSqlError e)
      then MonadCatch.throwM SqlSerializationError
      else MonadCatch.throwM e

unsafeMkSerializable :: ReaderT Pg.Connection (LoggingT IO) a -> Serializable a
unsafeMkSerializable = Serializable

unSerializable :: Serializable a -> ReaderT Pg.Connection (LoggingT IO) a
unSerializable (Serializable m) = m

toDbPersist :: forall a. Serializable a -> Hog.DbPersist Postgresql (LoggingT IO) a
toDbPersist (Serializable act) = Hog.DbPersist $ withReaderT coerce act

unsafeLiftDbPersist :: forall a. Hog.DbPersist Postgresql (LoggingT IO) a -> Serializable a
unsafeLiftDbPersist (Hog.DbPersist act) = Serializable $ withSqlSerializationErrorWrapping $ withReaderT coerce act

runSerializable :: forall a m. (MonadIO m) => Pool Pg.Connection -> LoggingEnv -> Serializable a -> m a
runSerializable pool logger (Serializable act) = liftIO $ withResource pool $ \c ->
  withTransactionModeRetry'
    (Pg.TransactionMode{ Pg.isolationLevel = Pg.Serializable, Pg.readWriteMode = Pg.ReadWrite})
    (\(_ :: SqlSerializationError) -> True)
    c
    (runLoggingEnv logger $ runReaderT act c)


-- | Like 'Pg.withTransactionModeRetry' but polymorphic over exception type.
-- Copied from https://github.com/phadej/postgresql-simple/blob/e02684f9c38acf736ac590b36b919000a2b45bc4/src/Database/PostgreSQL/Simple/Transaction.hs#L156-L174
withTransactionModeRetry' :: forall e a. E.Exception e => Pg.TransactionMode -> (e -> Bool) -> Pg.Connection -> IO a -> IO a
withTransactionModeRetry' mode shouldRetry conn act =
  E.mask $ \restore ->
    retryLoop $ E.try $ do
      a <- restore act `E.onException` rollback_ conn
      Pg.commit conn
      return a
  where
    retryLoop :: IO (Either e a) -> IO a
    retryLoop act' = do
      Pg.beginMode mode conn
      r <- act'
      case r of
        Left e -> case shouldRetry e of
          True -> retryLoop act'
          False -> E.throwIO e
        Right a -> return a

    -- | Rollback a transaction, ignoring any @IOErrors@
    rollback_ :: Pg.Connection -> IO ()
    rollback_ c = Pg.rollback c `E.catch` \(_ :: IOError) -> return ()
