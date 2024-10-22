{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Obelisk.Api where

import Prelude hiding ((.))

import Obelisk.Concurrency
import Obelisk.Postgres.Replication
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding

import Control.Category
import Control.Concurrent.Async
import Control.Concurrent.STM (TChan, atomically, readTChan, writeTChan, newTChanIO)
import Control.Monad (forever)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Default
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam (MonadBeam(..))
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Transaction as PG

newtype ReadDb a = ReadDb { unReadDb :: ReaderT (PG.Connection, Text -> IO ()) IO a }
  deriving (Functor, Applicative, Monad, MonadFail)

unsafePgToReadDb :: Pg a -> ReadDb a
unsafePgToReadDb x = ReadDb $ ReaderT $ \(conn, logger) -> runBeamPostgresDebug (logger . T.pack) conn x

instance MonadBeam Postgres ReadDb where
  runReturningMany cmd k = do
    logger <- ReadDb $ asks snd
    unsafePgToReadDb $ do
      let processItem item = liftIOWithHandle $ \innerConn -> do
            runReaderT (unReadDb $ k $ unsafePgToReadDb item) (innerConn, logger)
      runReturningMany cmd processItem
  runNoReturn cmd = unsafePgToReadDb (runNoReturn cmd)
  runReturningOne cmd = unsafePgToReadDb (runReturningOne cmd)
  runReturningList cmd = unsafePgToReadDb (runReturningList cmd)

readTransaction :: (Text -> IO ()) -> PG.Connection -> ReadDb a -> IO a
readTransaction logger conn (ReadDb r) = PG.withTransactionModeRetry m PG.isSerializationError conn $ runReaderT r (conn, logger)
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.Serializable
          , PG.readWriteMode = PG.ReadOnly
          }

readTransactionFromPool :: (Text -> IO ()) -> Pool PG.Connection -> ReadDb a -> IO a
readTransactionFromPool logger pool a = withResource pool $ \conn -> readTransaction logger conn a

newtype WriteDb a = WriteDb { unWriteDb :: ReaderT (PG.Connection, Text -> IO ()) IO a }
  deriving (Functor, Applicative, Monad, MonadFail)

pgToWriteDb :: Pg a -> WriteDb a
pgToWriteDb x = WriteDb $ ReaderT $ \(conn, logger) -> runBeamPostgresDebug (logger . T.pack) conn x

writeTransaction :: (Text -> IO ()) -> PG.Connection -> WriteDb a -> IO a
writeTransaction logger conn (WriteDb r) = PG.withTransactionModeRetry m PG.isSerializationError conn $ runReaderT r (conn, logger)
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.Serializable
          , PG.readWriteMode = PG.ReadWrite
          }

writeTransactionFromPool :: (Text -> IO ()) -> Pool PG.Connection -> WriteDb a -> IO a
writeTransactionFromPool logger pool a = withResource pool $ \conn -> writeTransaction logger conn a

unsafeReadDb :: ((PG.Connection, Text -> IO ()) -> IO a) -> ReadDb a
unsafeReadDb f = ReadDb $ lift . f =<< ask

unsafeWriteDb :: ((PG.Connection, Text -> IO ()) -> IO a) -> WriteDb a
unsafeWriteDb f = WriteDb $ lift . f =<< ask

withLogicalDecodingTransactions :: (Text -> IO ()) -> LogicalDecodingOptions -> ByteString -> (TChan (Either Message Transaction) -> IO a) -> IO a
withLogicalDecodingTransactions logger opts dbUri go = do
  withLogicalDecoding logger dbUri "test_decoding" opts $ \decodedLines -> do
    transactions :: TChan (Either Message Transaction) <- newTChanIO
    processLine <- linesToTransactions
    let processLines wdt = forever $ do
          wdt
          lineRaw <- atomically $ readTChan decodedLines
          l <- case parseOnly (line <* endOfInput) lineRaw of
            Right l -> pure l
            Left e -> do
              fail $ "Error while parsing replication message: " <> e
          Right mTransaction <- processLine l
          forM_ mTransaction $ \transaction -> do
            atomically $ writeTChan transactions transaction
    withSingleWorkerWatchdog logger "withLogicalDecodingTransactions-processLines" processLines $
      go transactions

withNonEmptyTransactions :: (Text -> IO ()) -> ByteString -> (TChan (Either Message Transaction) -> IO a) -> IO a
withNonEmptyTransactions logger = withLogicalDecodingTransactions logger $ def
  { _logicalDecodingOptions_pluginOptions = [("skip-empty-xacts", Nothing)]
  }

runRepeatableReadTransaction :: (Text -> IO ()) -> PG.Connection -> ReadDb a -> IO a
runRepeatableReadTransaction logger conn (ReadDb r) = PG.withTransactionMode m conn $ runReaderT r (conn, logger)
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.RepeatableRead
          , PG.readWriteMode = PG.ReadOnly
          }

beginRepeatableReadTransaction :: PG.Connection -> IO ()
beginRepeatableReadTransaction conn = PG.beginMode m conn
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.RepeatableRead
          , PG.readWriteMode = PG.ReadOnly
          }

-- | Run the given read-only action in a connection that is already in a transaction
unsafeRunInOpenWriteTransaction :: (Text -> IO ()) -> PG.Connection -> WriteDb a -> IO a
unsafeRunInOpenWriteTransaction logger conn (WriteDb r) = runReaderT r (conn, logger)

-- | Run the given read-only action in a connection that is already in a transaction
unsafeRunInOpenReadTransaction :: (Text -> IO ()) -> PG.Connection -> ReadDb a -> IO a
unsafeRunInOpenReadTransaction logger conn (ReadDb r) = runReaderT r (conn, logger)
