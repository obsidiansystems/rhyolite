{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Obelisk.Beam where

import Obelisk.Api

import Control.Monad.Reader
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full

class MonadBeamRead m where
  runSelectReturningList' :: FromBackendRow Postgres a => SqlSelect Postgres a -> m [a]
  runSelectReturningOne' :: FromBackendRow Postgres a => SqlSelect Postgres a -> m (Maybe a)

unsafeRunPgReadDb :: Pg a -> ReadDb a
unsafeRunPgReadDb a = ReadDb $ do
  (conn, logger) <- ask
  lift $ runBeamPostgresDebug (logger . T.pack) conn a

unsafeRunPgWriteDb :: Pg a -> WriteDb a
unsafeRunPgWriteDb a = WriteDb $ do
  (conn, logger) <- ask
  lift $ runBeamPostgresDebug (logger . T.pack) conn a

instance MonadBeamRead WriteDb where
  runSelectReturningList' = unsafeRunPgWriteDb . runSelectReturningList
  runSelectReturningOne' = unsafeRunPgWriteDb . runSelectReturningOne

instance MonadBeamRead ReadDb where
  runSelectReturningList' = unsafeRunPgReadDb . runSelectReturningList
  runSelectReturningOne' = unsafeRunPgReadDb . runSelectReturningOne

class MonadBeamWrite m where
  runInsert' :: SqlInsert Postgres table -> m ()
  runInsertReturningList' :: FromBackendRow Postgres a => PgInsertReturning a -> m [a]
  runUpdate' :: SqlUpdate Postgres table -> m ()
  runUpdateReturningList' :: FromBackendRow Postgres a => PgUpdateReturning a -> m [a]
  runDelete' :: SqlDelete Postgres table -> m ()
  runDeleteReturningList' :: FromBackendRow Postgres a => PgDeleteReturning a -> m [a]

instance MonadBeamWrite WriteDb where
  runInsert' = unsafeRunPgWriteDb . runInsert
  runInsertReturningList' = unsafeRunPgWriteDb . runPgInsertReturningList
  runUpdate' = unsafeRunPgWriteDb . runUpdate
  runUpdateReturningList' = unsafeRunPgWriteDb . runPgUpdateReturningList
  runDelete' = unsafeRunPgWriteDb . runDelete
  runDeleteReturningList' = unsafeRunPgWriteDb . runPgDeleteReturningList
