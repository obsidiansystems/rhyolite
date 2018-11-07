{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Backend.DB where

import Control.Arrow (first)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
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
import Database.Groundhog.Postgresql (SqlDb, isFieldNothing, runDbConn)

import Rhyolite.Backend.DB.PsqlSimple
import Rhyolite.Backend.Schema
import Rhyolite.Backend.Schema.Class
import Rhyolite.Schema

-- | Runs a database action using a given pool of database connections
-- The 'f' parameter can be used to represent additional information about
-- how/where to run the database action (e.g., in a particular schema).
-- See instances below.
class RunDb f where
  runDb :: ( MonadIO m
           , MonadBaseControl IO m
           , ConnectionManager cm conn
           , PostgresRaw (DbPersist conn m)
           , PersistBackend (DbPersist conn m))
        => f (Pool cm)
        -> DbPersist conn m b
        -> m b

-- | Runs a database action in the public schema
instance RunDb Identity where
  runDb (Identity db) = withResource db . runDbConn . withSchema (SchemaName "public")

-- | Runs a database action in a specific schema
instance RunDb WithSchema where
  runDb (WithSchema schema db) = withResource db . runDbConn . withSchema schema

getSearchPath :: PersistBackend m => m String
getSearchPath = do
  [searchPath] :: [String] <- queryRaw False "SHOW search_path" [] $ mapAllRows (fmap fst . fromPersistValues)
  return searchPath

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
  [] -> pure $ Nothing
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

getTime :: PersistBackend m => m UTCTime
getTime = do
  Just [PersistUTCTime t] <- queryRaw False "select current_timestamp(3) at time zone 'utc'" [] id
  return t

withTime :: PersistBackend m => (UTCTime -> m a) -> m a
withTime a = do
  now <- getTime
  a now

ilike :: (SqlDb db, ExpressionOf db r a a') => a -> String -> Cond db r
ilike a b = CondRaw $ operator 40 " ILIKE " a b
