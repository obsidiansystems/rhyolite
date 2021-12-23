{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}
{-|
Description : Utility functions for using Beam with Postgres Transactions.
-}
module Rhyolite.Db.Beam where

import Database.Beam
import Database.Beam.Postgres

import Database.PostgreSQL.Simple.Transaction

-- | Run beam SQL statements inside a Postgres Serializable Transaction
withTransactionSerializableRunBeamPostgres :: (MonadIO m) => Connection -> Pg a -> m a
withTransactionSerializableRunBeamPostgres dbConn = liftIO . withTransactionSerializable dbConn . runBeamPostgres dbConn

