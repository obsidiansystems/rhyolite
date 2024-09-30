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
import Data.UUID

import Rhyolite.DB.Beam.Orphans ()

-- | Run beam SQL statements inside a Postgres Serializable Transaction
withTransactionSerializableRunBeamPostgres :: (MonadIO m) => Connection -> Pg a -> m a
withTransactionSerializableRunBeamPostgres dbConn = liftIO . withTransactionSerializable dbConn . runBeamPostgres dbConn

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
current_timestamp_ :: QExpr Postgres s UTCTime
-- Note: Previously we used the SQL expression @current_timestamp at time zone 'UTC'@
--       here instead of @current_timestamp@.
--       This caused a bug because this expression returns the current UTC time
--       but without any time zone information; ie. the value would have the type
--       @TIMESTAMP WITHOUT TIME ZONE@. When a value of this type is inserted
--       into a column of type @TIMESTAMP WITH TIME ZONE@ it is cast into a value
--       of the latter type by assuming the time zone is whatever the Postgres
--       server is configured to use. This caused wrong timestamps to be inserted
--       for all Postgres server instances whose time zone was not configured to
--       be UTC.
--       The type of the expression @current_timestamp@ is @TIMESTAMP WITH TIME ZONE@,
--       which Postgres internally converts into a UTC timestamp which it stores.
--       When this is retrieved from the database, Postgres read the stored UTC
--       timestamp and returns the timestamp string using whatever time zone the
--       server is configured to use. This string is deserialized into a 'UTCTime'
--       (by 'postgresql-simple') by looking at the returned time zone offset and
--       adjusting to UTC.
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp"))

genRandomUuid_ :: QGenExpr ctxt Postgres s UUID
genRandomUuid_ = customExpr_ "gen_random_uuid()"
