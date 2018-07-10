{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.DB.LargeObjects
  ( PostgresLargeObject (..)
  , withStreamedLargeObject
  ) where

import Control.Exception.Lifted (AssertionFailed (..), bracket, throwIO)
import Control.Monad.Loops (whileJust_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State as State
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Semigroup ((<>))
import Database.Groundhog.Core (DbPersist)
import Database.Groundhog.Postgresql (Postgresql)
import Database.PostgreSQL.Simple.LargeObjects (LoFd, Oid (..))
import qualified Database.PostgreSQL.Simple.LargeObjects as Sql
import System.IO (IOMode (ReadMode, WriteMode))
import System.IO.Streams (InputStream, OutputStream, makeOutputStream)
import qualified System.IO.Streams as Streams

import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw (..), liftWithConn)
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
  withLargeObject oid mode f =
    bracket (liftWithConn $ \conn -> Sql.loOpen conn (toOid oid) mode)
            (\lofd -> liftWithConn $ \conn -> Sql.loClose conn lofd)
            f
  newLargeObjectFromFile filePath = do
    liftWithConn $ \conn -> fmap fromOid $ Sql.loImport conn filePath
  newLargeObjectBS contents = do
    oid <- newEmptyLargeObject
    n <- withLargeObject oid WriteMode $ \lofd -> liftWithConn $ \conn -> Sql.loWrite conn lofd contents
    let l = BS.length contents
    when (n /= l) . liftIO . throwIO . AssertionFailed $
      "newLargeObjectBS: loWrite reported writing " <> show n <> " bytes, expected " <> show l <> "."
    return oid
  newLargeObjectLBS = newLargeObjectStream <=< liftIO . Streams.fromLazyByteString
  newLargeObjectStream s = do
    oid <- newEmptyLargeObject
    t <- withLargeObject oid WriteMode $ \lofd -> do
      whileJust_ (liftIO $ Streams.read s) $ \chunk -> do
        n <- liftWithConn $ \conn -> Sql.loWrite conn lofd chunk
        let l = BS.length chunk
        when (n /= l) . throwIO . AssertionFailed $
          "newLargeObjectLBS: loWrite reported writing " <> show n <> " bytes, expected " <> show l <> "."
      liftWithConn $ \conn -> Sql.loTell conn lofd
    return (oid, t)
  streamLargeObject oid os =
    withLargeObject oid ReadMode $ \lofd ->
      fix $ \again -> do
        chunk <- readLargeObject lofd 8192 -- somewhat arbitrary
        case BS.length chunk of
          0 -> return ()
          _ -> do
            liftIO $ Streams.write (Just $ byteString chunk) os
            again
  streamLargeObjectRange oid start end os =
    withLargeObject oid ReadMode $ \lofd -> do
      _ <- liftWithConn $ \conn -> Sql.loSeek conn lofd Sql.AbsoluteSeek start
      let again n = do
            let nextChunkSize = min 8192 (end - n)
            chunk <- readLargeObject lofd nextChunkSize
            case BS.length chunk of
              0 -> return ()
              k -> do
                liftIO $ Streams.write (Just $ byteString chunk) os
                again (n + k)
      again start

  deleteLargeObject oid = liftWithConn $ \conn -> Sql.loUnlink conn $ toOid oid

-- Read a chunk of an opened large object. Returns Nothing when there's an error such as the end of file.
-- NB: postgresql-simple seems to have a less useful type here than postgresql-libpq...
readLargeObject :: MonadIO m => LoFd -> Int -> DbPersist Postgresql m BS.ByteString
readLargeObject lofd size = liftWithConn $ \conn -> Sql.loRead conn lofd size

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

withStreamedLargeObject :: (PostgresLargeObject m, MonadIO m) => LargeObjectId -> (LBS.ByteString -> IO ()) -> m ()
withStreamedLargeObject oid f = do
  lo <- liftIO $ newIORef mempty
  cb <- liftIO $ makeOutputStream $ \case
    Just chunk -> modifyIORef lo $ \chunks -> chunks <> chunk
    Nothing -> do
      payload <- readIORef lo
      f $ BS.toLazyByteString payload
  streamLargeObject oid cb
  liftIO $ Streams.write Nothing cb
