{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.DB.LargeObjects
  ( PostgresLargeObject (..)
  , withStreamedLargeObject
  ) where

import Control.Exception.Lifted (bracket)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as LBS
import Database.Groundhog.Core (DbPersist)
import Database.Groundhog.Postgresql (Postgresql)
import Database.PostgreSQL.Simple.LargeObjects (LoFd, Oid (..))
import qualified Database.PostgreSQL.Simple.LargeObjects.Stream as LO
import qualified Database.PostgreSQL.Simple.LargeObjects as Sql
import System.IO (IOMode)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw (..), liftWithConn)
import Rhyolite.Backend.DB.Serializable (Serializable, toDbPersist, unsafeLiftDbPersist)
import Rhyolite.Schema (LargeObjectId (..))

class PostgresRaw m => PostgresLargeObject m where
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

fromOid :: Oid -> LargeObjectId
fromOid (Oid n) = LargeObjectId (fromIntegral n)

toOid :: LargeObjectId -> Oid
toOid (LargeObjectId n) = Oid (fromIntegral n)

instance (MonadIO m, MonadBaseControl IO m) => PostgresLargeObject (DbPersist Postgresql m) where
  newEmptyLargeObject = fmap fromOid $ liftWithConn $ \conn -> Sql.loCreat conn
  withLargeObject oid mode =
    bracket (liftWithConn $ \conn -> Sql.loOpen conn (toOid oid) mode)
            (\lofd -> liftWithConn $ \conn -> Sql.loClose conn lofd)
  newLargeObjectFromFile filePath =
    liftWithConn $ \conn -> fmap fromOid $ Sql.loImport conn filePath
  newLargeObjectBS contents =
    fmap fromOid $ liftWithConn $ \conn -> LO.newLargeObjectBS conn contents
  newLargeObjectLBS =
    newLargeObjectStream <=< liftIO . Streams.fromLazyByteString
  newLargeObjectStream s = fmap (\(oid,sz) -> (fromOid oid, sz)) $
    liftWithConn $ \conn -> LO.newLargeObjectStream conn s
  streamLargeObject oid os =
    liftWithConn $ \conn -> LO.streamLargeObject conn (toOid oid) os
  streamLargeObjectRange oid start end os =
    liftWithConn (\conn -> LO.streamLargeObjectRange conn (toOid oid) start end os)

  deleteLargeObject oid = liftWithConn $ \conn -> Sql.loUnlink conn $ toOid oid

instance PostgresLargeObject Serializable where
  newEmptyLargeObject = unsafeLiftDbPersist newEmptyLargeObject
  withLargeObject oid mode f = unsafeLiftDbPersist $ withLargeObject oid mode (toDbPersist . f)
  newLargeObjectFromFile = unsafeLiftDbPersist . newLargeObjectFromFile
  newLargeObjectBS = unsafeLiftDbPersist . newLargeObjectBS
  newLargeObjectLBS = unsafeLiftDbPersist . newLargeObjectLBS
  newLargeObjectStream = unsafeLiftDbPersist . newLargeObjectStream
  streamLargeObject oid os = unsafeLiftDbPersist $ streamLargeObject oid os
  streamLargeObjectRange oid start end os = unsafeLiftDbPersist $ streamLargeObjectRange oid start end os
  deleteLargeObject = unsafeLiftDbPersist . deleteLargeObject

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

withStreamedLargeObject
  :: (MonadIO m)
  => LargeObjectId
  -> (LBS.ByteString -> IO ())
  -> DbPersist Postgresql m ()
withStreamedLargeObject oid f = liftWithConn $ \conn -> LO.withLargeObjectLBS conn (toOid oid) f
