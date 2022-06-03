{-# Language DefaultSignatures #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
module Database.PostgreSQL.Simple.Class where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)
import qualified Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Control.Monad.Trans.RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import qualified Control.Monad.Trans.Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Typeable
import Data.Word
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromRow (FromRow, RowParser)
import Database.PostgreSQL.Simple.LargeObjects (LoFd, Oid(..))
import qualified Database.PostgreSQL.Simple.LargeObjects.Stream as LO
import Database.PostgreSQL.Simple.SqlQQ.Interpolated
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.Types (Query)
import Language.Haskell.TH (appE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import System.IO (IOMode)
import System.IO.Streams (InputStream, OutputStream)

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

-- | Invokes 'query' with arguments provided by 'isql'
iquery :: QuasiQuoter
iquery = isql { quoteExp = appE [| uncurry query |] . quoteInterpolatedSql }

-- | Invokes 'execute' with arguments provided by 'isql'
iexecute :: QuasiQuoter
iexecute = isql { quoteExp = appE [| uncurry execute |] . quoteInterpolatedSql }

-- | Invokes 'execute_' with arguments provided by 'isql'
iexecute_ :: QuasiQuoter
iexecute_ = isql { quoteExp = appE [| uncurry execute_ |] . quoteInterpolatedSql }

-- | Invokes 'traceQuery' with arguments provided by 'isql'
itraceQuery :: QuasiQuoter
itraceQuery = isql { quoteExp = appE [| uncurry traceQuery |] . quoteInterpolatedSql }

-- | Invokes 'traceExecute' with arguments provided by 'isql'
itraceExecute :: QuasiQuoter
itraceExecute = isql { quoteExp = appE [| uncurry traceExecute |] . quoteInterpolatedSql }

-- | Invokes 'traceExecute_' with arguments provided by 'isql'
itraceExecute_ :: QuasiQuoter
itraceExecute_ = isql { quoteExp = appE [| uncurry traceExecute_ |] . quoteInterpolatedSql }

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

-- | Newtype for referring to database large objects. This generally shouldn't have to go over the wire
-- but I'm putting it here where it can be placed in types in the common schema, because often the Ids of
-- those types will want to be shared with the frontend. We're using Word64 here rather than CUInt, which
-- is the type that Oid wraps, because Word64 has Groundhog instances to steal.
newtype LargeObjectId = LargeObjectId Word64
  deriving (Eq, Ord, Show, Read, Typeable)

fromOid :: Oid -> LargeObjectId
fromOid (Oid n) = LargeObjectId (fromIntegral n)

toOid :: LargeObjectId -> Oid
toOid (LargeObjectId n) = Oid (fromIntegral n)

class PostgresLargeObject m where
  -- | Create a new postgres large object, returning its object id.
  newEmptyLargeObject :: m LargeObjectId
  default newEmptyLargeObject :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => m LargeObjectId
  newEmptyLargeObject = lift newEmptyLargeObject

  -- | Act on a large object given by id, opening and closing the file descriptor appropriately.
  withLargeObject :: LargeObjectId -> IOMode -> (LoFd -> m a) -> m a

  -- | Import a file into the database as a large object.
  newLargeObjectFromFile :: FilePath -> m LargeObjectId
  default newLargeObjectFromFile :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => FilePath -> m LargeObjectId
  newLargeObjectFromFile = lift . newLargeObjectFromFile

  -- | Given a strict ByteString, create a postgres large object and fill it with those contents.
  newLargeObjectBS :: BS.ByteString -> m LargeObjectId
  default newLargeObjectBS :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => BS.ByteString -> m LargeObjectId
  newLargeObjectBS = lift . newLargeObjectBS

  -- | Given a lazy ByteString, create a postgres large object and fill it with those contents.
  -- Also returns the total length of the data written.
  newLargeObjectLBS :: LBS.ByteString -> m (LargeObjectId, Int)
  default newLargeObjectLBS :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => LBS.ByteString -> m (LargeObjectId, Int)
  newLargeObjectLBS = lift . newLargeObjectLBS

  -- | Create a new large object from an input stream, returning its object id and overall size.
  newLargeObjectStream :: InputStream BS.ByteString -> m (LargeObjectId, Int)
  default newLargeObjectStream :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => InputStream BS.ByteString -> m (LargeObjectId, Int)
  newLargeObjectStream = lift . newLargeObjectStream

  -- | Stream the contents of a database large object to the given output stream. Useful with Snap's addToOutput.
  streamLargeObject :: LargeObjectId -> OutputStream Builder -> m ()
  default streamLargeObject :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => LargeObjectId -> OutputStream Builder -> m ()
  streamLargeObject oid os = lift $ streamLargeObject oid os

  -- | Stream the contents of a database large object to the given output stream. Useful with Snap's addToOutput.
  streamLargeObjectRange :: LargeObjectId -> Int -> Int -> OutputStream Builder -> m ()
  default streamLargeObjectRange :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => LargeObjectId -> Int -> Int -> OutputStream Builder -> m ()
  streamLargeObjectRange oid start end os = lift $ streamLargeObjectRange oid start end os

  -- | Deletes the large object with the specified object id.
  deleteLargeObject :: LargeObjectId -> m ()
  default deleteLargeObject :: (m ~ t m', MonadTrans t, PostgresLargeObject m', Monad m')
    => LargeObjectId -> m ()
  deleteLargeObject = lift . deleteLargeObject

withStreamedLargeObject
  :: (MonadIO m)
  => Connection
  -> LargeObjectId
  -> (LBS.ByteString -> IO ())
  -> m ()
withStreamedLargeObject conn oid f = liftIO $ LO.withLargeObjectLBS conn (toOid oid) f

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (StateT s m) where
  withLargeObject oid mode f = do
    s <- State.get
    (v,s') <- lift $ withLargeObject oid mode (\lofd -> runStateT (f lofd) s)
    put s'
    return v

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (Strict.StateT s m) where
  withLargeObject oid mode f = do
    s <- Strict.get
    (v,s') <- lift $ withLargeObject oid mode (\lofd -> Strict.runStateT (f lofd) s)
    put s'
    return v

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (MaybeT m) where
  withLargeObject oid mode f =
    MaybeT $ withLargeObject oid mode (\lofd -> runMaybeT (f lofd))

instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (ReaderT r m) where
  withLargeObject oid mode f = do
    s <- ask
    lift $ withLargeObject oid mode (\lofd -> runReaderT (f lofd) s)
