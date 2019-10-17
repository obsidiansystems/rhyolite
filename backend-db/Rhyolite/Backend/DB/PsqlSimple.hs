{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-deprecations #-}

module Rhyolite.Backend.DB.PsqlSimple
  ( PostgresRaw (..)
  , In (..), Only (..), Values (..)
  , Binary (..), (:.)(..), PGArray (..)
  , ToRow (..), FromRow (..)
  , ToField (..), FromField (..)
  , Query (..), sql, traceQuery, traceExecute, traceExecute_
  , liftWithConn
  , queryQ, executeQ, executeQ_, sqlQ, traceQueryQ, traceExecuteQ, traceExecuteQ_
  , fromIdRow
  ) where

import Control.Exception.Lifted (Exception, catch, throw)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Database.Groundhog.Postgresql (DbPersist (..), Postgresql (..))
import Database.Id.Class (Id(..), IdData)
import Database.PostgreSQL.Simple (Connection, SqlError)
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser, fromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField, Action)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.Types ((:.), Binary, In (..), Only (..), PGArray (..), Query, Values (..),
                                         fromQuery)
import Language.Haskell.TH (Exp, Name, Q, appE, mkName, tupE, varE, listE, sigE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import qualified Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.List
import qualified Control.Monad.Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict
import qualified Control.Monad.Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict
-- transformers >= 0.5.3
-- import qualified Control.Monad.Trans.Select
-- import qualified Control.Monad.Trans.Accum

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
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }

rethrowWithQueryMany :: ToRow q => Connection -> Query -> [q] -> SqlError -> IO a
rethrowWithQueryMany conn psql qs err = do
  expr <- Sql.formatMany conn psql qs
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = expr
    , _wrappedSqlError_error = err
    }
rethrowWithQuery_ :: Query -> SqlError -> IO a
rethrowWithQuery_ psql err =
  throw $ WrappedSqlError
    { _wrappedSqlError_rawQuery = fromQuery psql
    , _wrappedSqlError_formattedQuery = fromQuery psql
    , _wrappedSqlError_error = err
    }

class PostgresRaw m where
  execute :: ToRow q => Query -> q -> m Int64
  default execute :: ( m ~ t n , MonadTrans t , Monad n , PostgresRaw n , ToRow q) => Query -> q -> m Int64
  execute psql qs = lift $ execute psql qs

  execute_ :: Query -> m Int64
  default execute_ :: (m ~ t n, PostgresRaw n, Monad n, MonadTrans t) =>  Query -> m Int64
  execute_ = lift . execute_

  executeMany :: ToRow q => Query -> [q] -> m Int64
  default executeMany :: (m ~ t n, ToRow q, PostgresRaw n, Monad n, MonadTrans t) => Query -> [q] -> m Int64
  executeMany psql qs = lift $ executeMany psql qs

  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  default query :: (m ~ t n, ToRow q, FromRow r, PostgresRaw n, Monad n, MonadTrans t) => Query -> q -> m [r]
  query psql qs = lift $ query psql qs

  query_ :: FromRow r => Query -> m [r]
  default query_ :: (m ~ t n, FromRow r, PostgresRaw n, Monad n, MonadTrans t) => Query -> m [r]
  query_ = lift . query_

  queryWith :: ToRow q => RowParser r -> Query -> q -> m [r]
  default queryWith :: (m ~ t n, ToRow q, PostgresRaw n, Monad n, MonadTrans t) => RowParser r -> Query -> q -> m [r]
  queryWith parser psql qs = lift $ queryWith parser psql qs

  queryWith_ :: RowParser r -> Query -> m [r]
  default queryWith_ :: (m ~ t n, PostgresRaw n, Monad n, MonadTrans t) => RowParser r -> Query -> m [r]
  queryWith_ parser psql = lift $ queryWith_ parser psql

  formatQuery :: ToRow q => Query -> q -> m BS.ByteString
  default formatQuery :: (m ~ t n, ToRow q, PostgresRaw n, Monad n, MonadTrans t) => Query -> q -> m BS.ByteString
  formatQuery psql qs = lift $ formatQuery psql qs

  returning :: (ToRow q, FromRow r) => Query -> [q] -> m [r]
  default returning :: (m ~ t n, ToRow q, FromRow r, PostgresRaw n, Monad n, MonadTrans t) => Query -> [q] -> m [r]
  returning psql qs = lift $ returning psql qs


traceQuery :: (PostgresRaw m, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
traceQuery p q = do
  s <- formatQuery p q
  liftIO (BSC.putStrLn s)
  query p q

traceExecute :: (PostgresRaw m, MonadIO m, ToRow q) => Query -> q -> m Int64
traceExecute p q = do
  s <- formatQuery p q
  liftIO (BSC.putStrLn s)
  execute p q

traceExecute_ :: (PostgresRaw m, MonadIO m, ToRow q) => Query -> q -> m ()
traceExecute_ p q = void $ traceExecute p q

instance MonadIO m => PostgresRaw (DbPersist Postgresql m) where
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

instance (Monad m, PostgresRaw m) => PostgresRaw (StateT s m)
instance (Monad m, PostgresRaw m) => PostgresRaw (Strict.StateT s m)
instance (Monad m, PostgresRaw m) => PostgresRaw (MaybeT m)
instance (Monad m, PostgresRaw m) => PostgresRaw (ReaderT r m)

instance (Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.List.ListT m)
instance (Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Except.ExceptT e m)
instance (Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Identity.IdentityT m)
instance (Monoid w, Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Writer.Strict.WriterT w m)
instance (Monoid w, Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Writer.Lazy.WriterT w m)
instance (Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Cont.ContT r m)
instance (Monoid w, Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.RWS.Strict.RWST r w s m)
instance (Monoid w, Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.RWS.Lazy.RWST r w s m)
-- transformers >= 0.5.3
-- instance (Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Select.SelectT r m)
-- instance (Monoid w, Monad m, PostgresRaw m) => PostgresRaw (Control.Monad.Trans.Accum.AccumT w m)

---------------------------------
-- PostgreSQL.Simple instances --
---------------------------------

instance (FromField (IdData a)) => FromField (Id a) where
  fromField f mbs = fmap Id (fromField f mbs)

instance (ToField (IdData a)) => ToField (Id a) where
  toField (Id x) = toField x

defaultQQ :: String -> QuasiQuoter
defaultQQ name = QuasiQuoter
  { quotePat = error $ name <> ": quasiquoter used in pattern context"
  , quoteType = error $ name <> ": quasiquoter used in type context"
  , quoteExp = error $ name <> ": quasiquoter used in expression context"
  , quoteDec = error $ name <> ": quasiquoter used in declaration context"
  }

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'query'.
queryQ :: QuasiQuoter
queryQ = (defaultQQ "queryQ") { quoteExp = appE [| uncurry query |] . sqlQExp }

-- | This quasiquoter is the obvious combination of 'sqlQ' and 'execute'.
executeQ :: QuasiQuoter
executeQ = (defaultQQ "executeQ") { quoteExp = appE [| uncurry execute |] . sqlQExp }

executeQ_ :: QuasiQuoter
executeQ_ = (defaultQQ "executeQ_") { quoteExp = appE [| uncurry execute_ |] . sqlQExp }

traceQueryQ :: QuasiQuoter
traceQueryQ = (defaultQQ "traceQueryQ") { quoteExp = appE [| uncurry traceQuery |] . sqlQExp }

traceExecuteQ :: QuasiQuoter
traceExecuteQ = (defaultQQ "traceExecuteQ") { quoteExp = appE [| uncurry traceExecute |] . sqlQExp }

traceExecuteQ_ :: QuasiQuoter
traceExecuteQ_ = (defaultQQ "traceExecuteQ_") { quoteExp = appE [| uncurry traceExecute_ |] . sqlQExp }


-- | This quasiquoter takes a SQL query with named arguments in the form "?var" and generates a pair
-- consisting of the Query string itself and a tuple of variables in corresponding order.
--
-- For example: uncurry query [sqlQ| SELECT * FROM 'Book' b WHERE b.title = ?title AND b.author = ?author |]
--
-- will be equivalent to query [sql| SELECT * FROM 'Book' b WHERE b.title = ? AND b.author = ? |] [toField title, toField author]
sqlQ :: QuasiQuoter
sqlQ = (defaultQQ "sqlQ") { quoteExp = sqlQExp }

sqlQExp :: String -> Q Exp
sqlQExp s =
  let (s',vs) = extractVars s
  in tupE [quoteExp sql s', sigE (listE $ map (appE (varE 'toField) . varE) vs) [t| [Action] |]]

extractVars :: String -> (String, [Name])
extractVars = extractVars'
  where
    extractVars' [] = ([],[])
    extractVars' ('?':s') =
      let [(var,rest)] = lex s'
          (s'',vars) = extractVars' rest
      in ('?':s'', mkName var : vars)
    extractVars' s' =
      let (pre,post) = break (=='?') s'
          (s'',vars) = extractVars' post
      in (pre ++ s'', vars)

fromIdRow :: (Only (Id v) :. v) -> (Id v, v)
fromIdRow (Only k Sql.:. v) = (k, v)
