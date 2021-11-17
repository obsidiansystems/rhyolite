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
  ( Psql (..)
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
import Database.PostgreSQL.Simple.Class
import Control.Monad.Reader (ask)
import Control.Monad.State as State
import qualified Data.ByteString as BS
import Data.Coerce
import Database.Groundhog.Postgresql (DbPersist (..), Postgresql (..))
import Database.Id.Class (Id(..), IdData)
import Database.PostgreSQL.Simple (Connection, SqlError)
import qualified Database.PostgreSQL.Simple as Sql
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField, toField, Action)
import Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import Database.PostgreSQL.Simple.Types ((:.), Binary, In (..), Only (..), PGArray (..), Query, Values (..),
                                         fromQuery)
import Language.Haskell.TH (Exp, Name, Q, appE, mkName, tupE, varE, listE, sigE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

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
