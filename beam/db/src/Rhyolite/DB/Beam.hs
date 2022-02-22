{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}
{-|
Description : Utility functions for using Beam with Postgres Transactions.
-}
module Rhyolite.DB.Beam where

import Data.Time
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.Transaction

-- | Run beam SQL statements inside a Postgres Serializable Transaction
withTransactionSerializableRunBeamPostgres :: (MonadIO m) => Connection -> Pg a -> m a
withTransactionSerializableRunBeamPostgres dbConn = liftIO . withTransactionSerializable dbConn . runBeamPostgres dbConn

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
current_timestamp_ :: QExpr Postgres s UTCTime
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp at time zone 'UTC'"))
