{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Rhyolite.Backend.DB where

import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (LoggingT, MonadLoggerIO (askLoggerIO), NoLoggingT, runLoggingT)
-- import Control.Monad.Trans.Accum (AccumT) -- not MonadTransControl yet
import Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl, StM, StT)
import Control.Monad.Trans.Error (Error, ErrorT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
-- import qualified Control.Monad.Trans.RWS.CPS as CPS (RWST) -- only in newer transformers
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
-- import qualified Control.Monad.Trans.Writer.CPS as CPS (WriterT) -- only in newer transformers
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)
import Data.Coerce (Coercible, coerce)
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap, pattern MonoidalMap)
import Data.Maybe (listToMaybe)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Data.Time (UTCTime)
import Database.Groundhog.Core
import Database.Groundhog.Expression (Expression, ExpressionOf, Unifiable)
import Database.Groundhog.Generic (mapAllRows)
import Database.Groundhog.Generic.Sql (operator)
import Database.Groundhog.Postgresql (Postgresql (..), SqlDb, isFieldNothing, in_)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Database.Id.Class
import Database.Id.Groundhog

import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.DB.Serializable (Serializable, runSerializable)
import Rhyolite.Backend.Schema ()
import Rhyolite.Logging (LoggingEnv (..))
import Rhyolite.Schema

type Db m = (PersistBackend m, PostgresRaw m, SqlDb (PhantomDb m))

runDbPersistTransactionMode
  :: (MonadIO m, MonadLoggerIO m)
  => Pg.IsolationLevel
  -> Pg.ReadWriteMode
  -> Pool Pg.Connection
  -> DbPersist Postgresql (LoggingT IO) a
  -> m a
runDbPersistTransactionMode isoLevel rwMode dbPool (DbPersist act) = do
  logger <- askLoggerIO
  liftIO $ withResource dbPool $ \conn ->
    Pg.withTransactionMode (Pg.TransactionMode isoLevel rwMode) conn $
      runLoggingT (runReaderT act (coerce conn)) logger

-- | Runs a database action using a given pool of database connections
-- The 'f' parameter can be used to represent additional information about
-- how/where to run the database action (e.g., in a particular schema).
-- See instances below.
class RunDb f where
  -- | Runs a transaction at serializable isolation level and automatically retries
  --   in case of a serialization error.
  runDb :: (MonadIO m, MonadLoggerIO m, Coercible conn Pg.Connection)
        => f (Pool conn)
        -> Serializable a
        -> m a

  -- | Runs a read-only transaction at repeatable read isolation level.
  --   No retrying is done.
  --
  --  Note that a read-only repeatable read transaction is the highest
  --  isolation level we can get that cannot fail due to a
  --  serialization error and cannot be starved by other transactions.
  --  For contrast, @SERIALIZABLE READ ONLY DEFERRED@ is the highest
  --  isolation that cannot fail due to serialization error but it
  --  can be starved.
  --
  --  NB: "Read-only" is not statically checked!
  runDbReadOnlyRepeatableRead
    :: (MonadIO m, MonadLoggerIO m, Coercible conn Pg.Connection)
    => f (Pool conn)
    -> DbPersist Postgresql (LoggingT IO) a
    -> m a

-- | Runs a database action in the public schema
instance RunDb Identity where
  runDb (Identity db) act = do
    logger <- askLoggerIO
    runSerializable (coerce db) (LoggingEnv logger) $ withSchema (SchemaName "public") act
  runDbReadOnlyRepeatableRead (Identity db) =
    runDbPersistTransactionMode Pg.RepeatableRead Pg.ReadOnly (coerce db)

-- | Runs a database action in a specific schema
instance RunDb WithSchema where
  runDb (WithSchema schema db) act = do
    logger <- askLoggerIO
    runSerializable (coerce db) (LoggingEnv logger) $ withSchema schema act
  runDbReadOnlyRepeatableRead (WithSchema schema db) =
    runDbPersistTransactionMode Pg.RepeatableRead Pg.ReadOnly (coerce db) . withSchema schema

getSearchPath :: PersistBackend m => m String
getSearchPath =
  queryRaw False "SHOW search_path" [] (mapAllRows (fmap fst . fromPersistValues)) >>= \case
    [searchPath] -> return searchPath
    _ -> error "getSearchPath: Unexpected result from queryRaw"

setSearchPath :: (Monad m, PostgresRaw m) => String -> m ()
setSearchPath sp = void $ execute_ $ "SET search_path TO " `mappend` fromString sp

setSchema :: (Monad m, PostgresRaw m) => SchemaName -> m ()
setSchema schema = void $ execute [sql| SET search_path TO ?,"$user",public |] (Only schema)

-- | Sets the search path to a particular schema, runs an action in that schema, and resets the search path
withSchema
  :: (PostgresRaw m, PersistBackend m)
  => SchemaName
  -> m r
  -> m r
withSchema schema a = do
  sp <- getSearchPath
  setSchema schema
  r <- a
  setSearchPath sp
  return r

ensureSchemaExists
  :: (Monad m, PostgresRaw m)
  => SchemaName
  -> m ()
ensureSchemaExists schema = void $ execute [sql| CREATE SCHEMA IF NOT EXISTS ? |] (Only schema)

-- | Convenience function for getting the first result of a projection as a 'Maybe'
project1
  :: ( PersistEntity v, EntityConstr v c
     , Projection' p conn (RestrictionHolder v c) a
     , HasSelectOptions opts conn (RestrictionHolder v c)
     , HasLimit opts ~ HFalse
     , PersistBackend m, ProjectionDb p conn, PhantomDb m ~ conn )
  => p -> opts -> m (Maybe a)
project1 p opts = fmap listToMaybe $ project p $ opts `limitTo` 1

-- | Convenience function for getting the first result of a projection. Calls
-- 'error' when there is no result.
project1'
  :: ( PersistEntity v, EntityConstr v c
     , Projection' p conn (RestrictionHolder v c) a
     , HasSelectOptions opts conn (RestrictionHolder v c)
     , HasLimit opts ~ HFalse
     , PersistBackend m, ProjectionDb p conn, PhantomDb m ~ conn )
  => p -> opts -> m a
project1' p opts = project1 p opts >>= \case
  Nothing -> error "project1' expected a result, but got none"
  Just a -> pure a

-- | 'select', limited to one, with a haskell failure if the DB doesn't
-- so limit it.
selectSingle
  :: ( PersistEntity v, EntityConstr v c, PersistBackend m
     , HasSelectOptions opts (PhantomDb m) (RestrictionHolder v c)
     , HasLimit opts ~ HFalse )
  => opts
  -> m (Maybe v)
selectSingle cond = select (cond `limitTo` 1) >>= \case
  [] -> pure Nothing
  [x] -> pure $ Just x
  _   -> fail "PostgreSQL ignored LIMIT TO 1"

-- | Will return all matching instances of the given constructor
selectMap
  :: forall opts (m :: * -> *) v (c :: (* -> *) -> *) t.
     ( ProjectionDb t (PhantomDb m)
     , ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v
     , Projection t v, EntityConstr v c
     , HasSelectOptions opts (PhantomDb m) (RestrictionHolder v c)
     , PersistBackend m, Ord (IdData v), AutoKey v ~ DefaultKey v)
  => t -- ^ Constructor
  -> opts -- ^ Select options
  -> m (Map (Id v) v)
selectMap constr = fmap (Map.fromList . map (first toId)) . project (AutoKeyField, constr)

selectMap'
  :: forall a (m :: * -> *) v (c :: (* -> *) -> *) t.
     ( ProjectionDb t (PhantomDb m)
     , ProjectionRestriction t (RestrictionHolder v c), DefaultKeyId v
     , Projection t v, EntityConstr v c
     , HasSelectOptions a (PhantomDb m) (RestrictionHolder v c)
     , PersistBackend m, Ord (IdData v), AutoKey v ~ DefaultKey v)
  => t -- ^ Constructor
  -> a -- ^ Select options
  -> m (MonoidalMap (Id v) v)
selectMap' constr = fmap MonoidalMap . selectMap constr

fieldIsJust, fieldIsNothing
  :: ( NeverNull a, Expression db r f, PrimitivePersistField a
     , Projection f (Maybe a), Unifiable f (Maybe a))
  => f
  -> Cond db r
fieldIsJust f = Not $ isFieldNothing f
fieldIsNothing = isFieldNothing

getTime :: (PersistBackend m) => m UTCTime
getTime =
  queryRaw False "select current_timestamp(3) at time zone 'utc'" [] id >>= \case
    Just [PersistUTCTime t] -> return t
    _ -> error "getTime: Unexpected result of queryRaw"

withTime :: PersistBackend m => (UTCTime -> m a) -> m a
withTime a = do
  now <- getTime
  a now

ilike :: (SqlDb db, ExpressionOf db r a a') => a -> String -> Cond db r
ilike a b = CondRaw $ operator 40 " ILIKE " a b

-- | This is an alternative to groundhog's `==.`. It's useful because groundhog < 0.10 will turn `==.` into
-- "IS NOT DISTINCT FROM", which has terrible performance and doesn't use indexes.
(===.) ::  (SqlDb db, PrimitivePersistField b, Expression db r a, Expression db r b, Unifiable a b) => a -> b -> Cond db r
(===.) a b = a `in_` [b]

class MonadTransControl t => MonadTransNoPureAborts t where
  -- | This is basically a 'soft proof' that a transformer preserves the characteristic property
  --   of 'MonadBaseNoPureAborts'.  It is intended to serve as a stumbling block to warn a user
  --   who tries to construct an incorrect instance, rather than an airtight proof or a method
  --   for actual use.  In particular, it's not that good at detecting reuse of continuations.
  --
  --   The proxy parameter is required because StT is not injective.
  noPureAbortsT :: proxy t -> StT t a -> a

newtype Flip f a b = Flip (f b a)

-- | Groundhog invokes a transaction using the pattern 'begin >> (txn >> commit) `onException` abort'.
--   This pattern assumes some things about the monad that are not captured by 'MonadBaseControl IO'.
--   'MonadBaseNoPureAborts' exists to capture these assumptions: namely, that there is no reuse or
--   discarding of continuations except via effects in the base monad.
--
--   An instance could exist for DbPersist, but this is purposely omitted in order to forbid attempts
--   to nest transactions.
--
--   Instances should either be 'trivial' like the IO instance, or they should be default instances.
class MonadBaseControl n m => MonadBaseNoPureAborts n m where
  noPureAborts :: proxy m -> StM m a -> a
  default noPureAborts :: forall t m' a proxy. (m ~ t m', StM m a ~ StM m' (StT t a), MonadBaseNoPureAborts n m', MonadTransNoPureAborts t) => proxy m -> StM m a -> a
  noPureAborts p = noPureAbortsT (Flip $ Compose p) . noPureAborts (Compose p)

instance MonadBaseNoPureAborts Identity Identity where
  noPureAborts _ = id

instance MonadBaseNoPureAborts IO IO where
  noPureAborts _ = id

-- instance MonadTransNoPureAborts (AccumT w) where
--   noPureAbortsT _ = fst
-- instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (AccumT w m)

-- better error message
instance (Error e, MonadBaseNoPureAborts n m, MonadTransNoPureAborts (ErrorT e)) => MonadBaseNoPureAborts n (ErrorT e m)
instance (MonadBaseNoPureAborts n m, MonadTransNoPureAborts (ExceptT e)) => MonadBaseNoPureAborts n (ExceptT e m)
instance (MonadBaseNoPureAborts n m, MonadTransNoPureAborts ListT) => MonadBaseNoPureAborts n (ListT m)

instance MonadTransNoPureAborts LoggingT where
  noPureAbortsT _ = id
instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (LoggingT m)

instance MonadTransNoPureAborts NoLoggingT where
  noPureAbortsT _ = id
instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (NoLoggingT m)

-- better error message
instance (MonadBaseNoPureAborts n m, MonadTransNoPureAborts MaybeT) => MonadBaseNoPureAborts n (MaybeT m)

instance MonadTransNoPureAborts (ReaderT r) where
  noPureAbortsT _ = id
instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (ReaderT r m)

-- instance Monoid w => MonadTransNoPureAborts (CPS.RWST r w s) where
--   noPureAbortsT _ (x,_,_) = x
-- instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (CPS.RWST r w s m)
instance Monoid w => MonadTransNoPureAborts (Lazy.RWST r w s) where
  noPureAbortsT _ (x,_,_) = x
instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (Lazy.RWST r w s m)
instance Monoid w => MonadTransNoPureAborts (Strict.RWST r w s) where
  noPureAbortsT _ (x,_,_) = x
instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (Strict.RWST r w s m)

instance MonadTransNoPureAborts (Lazy.StateT s) where
  noPureAbortsT _ = fst
instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (Lazy.StateT s m)
instance MonadTransNoPureAborts (Strict.StateT s) where
  noPureAbortsT _ = fst
instance MonadBaseNoPureAborts n m => MonadBaseNoPureAborts n (Strict.StateT s m)

-- instance Monoid w => MonadTransNoPureAborts (CPS.WriterT w) where
--   noPureAbortsT _ = fst
-- instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (CPS.WriterT w m)
instance Monoid w => MonadTransNoPureAborts (Lazy.WriterT w) where
  noPureAbortsT _ = fst
instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (Lazy.WriterT w m)
instance Monoid w => MonadTransNoPureAborts (Strict.WriterT w) where
  noPureAbortsT _ = fst
instance (Monoid w, MonadBaseNoPureAborts n m) => MonadBaseNoPureAborts n (Strict.WriterT w m)
