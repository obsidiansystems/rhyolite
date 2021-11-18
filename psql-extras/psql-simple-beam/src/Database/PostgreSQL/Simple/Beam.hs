{-# options_ghc -fno-warn-orphans #-}
module Database.PostgreSQL.Simple.Beam where

import Database.PostgreSQL.Simple.Class
import qualified Database.PostgreSQL.Simple as P
import Database.Beam.Postgres

instance Psql Pg where
  askConn = liftIOWithHandle pure
  execute a b = liftIOWithHandle $ \c -> P.execute c a b
  execute_ a = liftIOWithHandle $ \c -> P.execute_ c a
  executeMany a b = liftIOWithHandle $ \c -> P.executeMany c a b
  query a b = liftIOWithHandle $ \c -> P.query c a b
  query_ a = liftIOWithHandle $ \c -> P.query_ c a
  queryWith foo bar baz = liftIOWithHandle $ \c -> P.queryWith foo c bar baz
  queryWith_ foo bar = liftIOWithHandle $ \c -> P.queryWith_ foo c bar
  formatQuery a b = liftIOWithHandle $ \c -> P.formatQuery c a b
  returning a b = liftIOWithHandle $ \c -> P.returning c a b
