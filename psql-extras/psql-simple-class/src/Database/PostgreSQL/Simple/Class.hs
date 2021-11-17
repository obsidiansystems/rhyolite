{-# Language DefaultSignatures #-}
{-# Language GADTs #-}
module Database.PostgreSQL.Simple.Class where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import qualified Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict
import qualified Control.Monad.Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser)
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types (Query)

class Monad m => Psql m where
  askConn :: m Connection
  default askConn :: ( m ~ t n, MonadTrans t, Monad n, Psql n) => m Connection
  askConn = lift askConn
  execute :: ToRow q => Query -> q -> m Int64
  default execute :: ( m ~ t n , MonadTrans t , Monad n , Psql n , ToRow q) => Query -> q -> m Int64
  execute psql qs = lift $ execute psql qs

  execute_ :: Query -> m Int64
  default execute_ :: (m ~ t n, Psql n, Monad n, MonadTrans t) =>  Query -> m Int64
  execute_ = lift . execute_

  executeMany :: ToRow q => Query -> [q] -> m Int64
  default executeMany :: (m ~ t n, ToRow q, Psql n, Monad n, MonadTrans t) => Query -> [q] -> m Int64
  executeMany psql qs = lift $ executeMany psql qs

  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  default query :: (m ~ t n, ToRow q, FromRow r, Psql n, Monad n, MonadTrans t) => Query -> q -> m [r]
  query psql qs = lift $ query psql qs

  query_ :: FromRow r => Query -> m [r]
  default query_ :: (m ~ t n, FromRow r, Psql n, Monad n, MonadTrans t) => Query -> m [r]
  query_ = lift . query_

  queryWith :: ToRow q => RowParser r -> Query -> q -> m [r]
  default queryWith :: (m ~ t n, ToRow q, Psql n, Monad n, MonadTrans t) => RowParser r -> Query -> q -> m [r]
  queryWith parser psql qs = lift $ queryWith parser psql qs

  queryWith_ :: RowParser r -> Query -> m [r]
  default queryWith_ :: (m ~ t n, Psql n, Monad n, MonadTrans t) => RowParser r -> Query -> m [r]
  queryWith_ parser psql = lift $ queryWith_ parser psql

  formatQuery :: ToRow q => Query -> q -> m BS.ByteString
  default formatQuery :: (m ~ t n, ToRow q, Psql n, Monad n, MonadTrans t) => Query -> q -> m BS.ByteString
  formatQuery psql qs = lift $ formatQuery psql qs

  returning :: (ToRow q, FromRow r) => Query -> [q] -> m [r]
  default returning :: (m ~ t n, ToRow q, FromRow r, Psql n, Monad n, MonadTrans t) => Query -> [q] -> m [r]
  returning psql qs = lift $ returning psql qs


traceQuery :: (Psql m, MonadIO m, ToRow q, FromRow r) => Query -> q -> m [r]
traceQuery p q = do
  s <- formatQuery p q
  liftIO (BSC.putStrLn s)
  query p q

traceExecute :: (Psql m, MonadIO m, ToRow q) => Query -> q -> m Int64
traceExecute p q = do
  s <- formatQuery p q
  liftIO (BSC.putStrLn s)
  execute p q

traceExecute_ :: (Psql m, MonadIO m, ToRow q) => Query -> q -> m ()
traceExecute_ p q = void $ traceExecute p q

instance (Monad m, Psql m) => Psql (StateT s m)
instance (Monad m, Psql m) => Psql (Strict.StateT s m)
instance (Monad m, Psql m) => Psql (MaybeT m)
instance (Monad m, Psql m) => Psql (ReaderT r m)

instance (Monad m, Psql m) => Psql (Control.Monad.Trans.Except.ExceptT e m)
instance (Monad m, Psql m) => Psql (Control.Monad.Trans.Identity.IdentityT m)
instance (Monoid w, Monad m, Psql m) => Psql (Control.Monad.Trans.Writer.Strict.WriterT w m)
instance (Monoid w, Monad m, Psql m) => Psql (Control.Monad.Trans.Writer.Lazy.WriterT w m)
instance (Monad m, Psql m) => Psql (Control.Monad.Trans.Cont.ContT r m)
instance (Monoid w, Monad m, Psql m) => Psql (Control.Monad.Trans.RWS.Strict.RWST r w s m)
instance (Monoid w, Monad m, Psql m) => Psql (Control.Monad.Trans.RWS.Lazy.RWST r w s m)
