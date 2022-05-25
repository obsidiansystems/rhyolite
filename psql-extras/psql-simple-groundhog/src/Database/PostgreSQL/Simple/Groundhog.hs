{-# Language FlexibleInstances #-}
{-# options_ghc -fno-warn-orphans #-}
module Database.PostgreSQL.Simple.Groundhog where

import Control.Exception.Lifted (Exception, catch, throw)
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.Coerce
import Database.Groundhog
import Database.Groundhog.Postgresql
import Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.Types as Sql
import Database.PostgreSQL.Simple.Class

instance MonadIO m => Psql (DbPersist Postgresql m) where
  askConn = coerce <$> DbPersist ask
  execute psql qs = liftWithConn $ \conn ->
    Sql.execute conn psql qs `catch` rethrowWithQuery conn psql qs
  execute_ psql = liftWithConn $ \conn -> Sql.execute_ conn psql `catch` rethrowWithQuery_ psql
  executeMany psql qs = liftWithConn $ \conn -> Sql.executeMany conn psql qs `catch` rethrowWithQueryMany conn psql qs
  query psql qs = liftWithConn $ \conn -> Sql.query conn psql qs `catch` rethrowWithQuery conn psql qs
  query_ psql = liftWithConn $ \conn -> Sql.query_ conn psql `catch` rethrowWithQuery_ psql
  queryWith parser psql qs = liftWithConn $ \conn -> Sql.queryWith parser conn psql qs `catch` rethrowWithQuery_ psql
  queryWith_ parser psql = liftWithConn $ \conn -> Sql.queryWith_ parser conn psql `catch` rethrowWithQuery_ psql
  formatQuery psql qs = liftWithConn $ \conn -> Sql.formatQuery conn psql qs
  returning psql qs = liftWithConn $ \conn -> Sql.returning conn psql qs `catch` rethrowWithQueryMany conn psql qs

liftWithConn :: MonadIO m
             => (Connection -> IO a)
             -> DbPersist Postgresql m a
liftWithConn f = DbPersist $ do
  (Postgresql conn) <- ask
  liftIO (f conn)

data WrappedSqlError = WrappedSqlError
  { _wrappedSqlError_rawQuery :: BS.ByteString
  , _wrappedSqlError_formattedQuery :: BS.ByteString
  , _wrappedSqlError_error :: SqlError
  }
  deriving Show

instance Exception WrappedSqlError

rethrowWithQuery :: ToRow q => Connection -> Query -> q -> SqlError -> IO a
rethrowWithQuery conn psql qs err = do
  expr <- Sql.formatQuery conn psql qs
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = Sql.fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }

rethrowWithQueryMany :: ToRow q => Connection -> Query -> [q] -> SqlError -> IO a
rethrowWithQueryMany conn psql qs err = do
  expr <- Sql.formatMany conn psql qs
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = Sql.fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }
rethrowWithQuery_ :: Query -> SqlError -> IO a
rethrowWithQuery_ psql err =
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = Sql.fromQuery psql
    , _wrappedSqlError_formattedQuery = Sql.fromQuery psql
    , _wrappedSqlError_error = err
    }

