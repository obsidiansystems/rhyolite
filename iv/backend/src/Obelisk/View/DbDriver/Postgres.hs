{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Obelisk.View.DbDriver.Postgres where

import Prelude hiding ((.))

import Control.Category
import Control.Concurrent.Classy.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad hiding (fail)
import Control.Monad.Ref.Restricted
import Data.Attoparsec.ByteString.Char8 (parseOnly, endOfInput)
import Data.ByteString (ByteString)
import Data.Default
import Data.Functor.Misc
import Data.Int
import Data.Patch.MapWithPatchingMove
import Data.Pool
import Data.Proxy
import Data.Text (Text)
import Database.Beam
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import Debug.Trace
import Obelisk.Api
import Obelisk.Beam.Patch.Db
import Obelisk.Beam.Patch.Decode.Postgres
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.Concurrency
import Obelisk.Db
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
import Obelisk.Postgres.Snapshot
-- import Obelisk.View.BeamQuery
import Obelisk.View.Postgres
-- import Obelisk.View.Unitary
import qualified Control.Concurrent.STM as Unclassy
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PG

traceQuery :: (Monad m, Show a) => (SqlSelect Postgres a -> m [a]) -> SqlSelect Postgres a -> m [a]
traceQuery fn q@(SqlSelect (PgSelectSyntax s)) = do
  traceM $ LChar8.unpack $ "traceQuery SQL:" <> (pgRenderSyntaxScript s)
  results <- fn q
  -- traverse_ (traceM . ("traceQuery ROW:" <>) . show) results
  traceM $ "traceQuery #" <> show (length results)
  return results

snapshotFencePrefix :: ByteString
snapshotFencePrefix = "obelisk/snapshotFence"

withDbDriver
  :: forall db a . _ => (Text -> IO ()) -> ByteString -> AnnotatedDatabaseSettings Postgres db -> (DbDriver db IO -> IO a) -> IO a
withDbDriver logger dbUri annotatedDb go = withConnectionPool dbUri $ \pool -> withResource pool $ \fenceConn -> withResource pool $ \decodingConn -> do
  readerIdVar <- newRef (1 :: Int)
  logger "withDbDriver - dedicated connections allocated from pool, starting loops"
  let db = deAnnotateDatabase annotatedDb
      dbDriver = DbDriver
        { _dbDriver_withFeed = \sendTransaction releaseSnapshot innerGo -> do
            withLogicalDecodingTransactions logger def dbUri $ \transactions -> do
              let feedTransactions wdt = forever $ do
                    wdt
                    Unclassy.atomically (Unclassy.readTChan transactions) >>= \case
                      Left msg -> do
                        when (_message_prefix msg == snapshotFencePrefix) $ do
                          case parseOnly (txidSnapshotParser <* endOfInput) $ _message_content msg of
                            Right s -> releaseSnapshot s
                            Left l -> fail $ "Error decoding fence snapshot: " <> l
                      Right (myXid, changes) -> do
                        TablesV patch <- decodeTransaction db decodingConn changes
                        let filterEmpty (TablePatch p@(PatchMapWithPatchingMove m)) = ComposeMaybe $
                              if null m
                              then Nothing
                              else Just $ TablePatch p
                        sendTransaction (myXid, QueryResultPatch $ TablesV $ mapTablesOnly filterEmpty patch)
              withSingleWorkerWatchdog logger "withDbDriver-feedTransactions" feedTransactions innerGo
        , _dbDriver_openReader = do
            readerId <- atomicModifyRef' readerIdVar $ \old -> (succ old, old)
            let putMyLog x = logger $ "reader " <> tshow readerId <> ": " <> x
            putMyLog "Starting reader"
            (conn, localPool) <- takeResource pool
            putMyLog "Got connection from pool"
            putMyLog "Allocated ID"
            beginRepeatableReadTransaction conn
            putMyLog "Began transaction"
            [Only (txidSnapshot :: TxidSnapshot)] <- PG.query_ conn [sql| select txid_current_snapshot() |]
            putMyLog $ "Got current snapshot: " <> tshow txidSnapshot
            -- Send a message letting obelisk know that the given snapshot is definitely all in the WAL by the time this message is received
            -- The goal here is to realize that certain transactions are empty, even though we never receive an explicit empty transaction from the logical decoding plugin.  This can occur, for example, whenever a transaction allocates a txid but then does not actually make any changes.
            --NOTE: The fence needs to be sent on another connection, because non-transactional pg_logical_emit_message commands do not promptly propagate to replication connections when sent from inside a transaction.  They *do* appear to propagate promptly when sent from *outside* a transaction, so that's what we do here.  We can't do it on the same connection, because we're in the transaction already.
            [Only (lsn :: Text)] <- PG.query fenceConn [sql| SELECT pg_logical_emit_message(false, ?, ?)::text; |] (snapshotFencePrefix, txidSnapshot)
            putMyLog $ "Emitted fence at " <> lsn <> "; ready for queries"
            connVar <- newMVar conn
            let reader = DbReader
                  { _dbReader_getVisibleTransactionsSince = \prevSnapshot snapshot -> withMVar connVar $ \connFromVar ->  do
                      putMyLog "Getting visible transactions"
                      let potentialXids = transactionsBetweenSnapshots prevSnapshot snapshot
                      xids :: [Only Int64] <- PG.query connFromVar [sql| select a.a from (?) a (a) where txid_status(a.a) = 'committed' |] $ Only (Values [ "int8" ] $ Only @Int64 . fromIntegral . unXid <$> Set.toList potentialXids)
                      let result = Set.fromList $ Xid . fromIntegral . fromOnly <$> xids
                      putMyLog $ "Got " <> tshow (Set.size result) <> " visible transactions"
                      pure result
                  , _dbReader_close = do
                      -- _not_ using the MVar so we force the commit if some query is slow.
                      putMyLog "Committing"
                      PG.commit conn
                      putMyLog "Committed; returning to pool"
                      putResource localPool conn
                      putMyLog "Returned to pool"
                  , _dbReader_runRead = \(AsyncReadDb qs0 sendRsp) -> do
                      putMyLog "Entering subtransaction"
                      withMVar connVar $ \connFromVar -> do
                        sp <- PG.newSavepoint connFromVar
                        rv <- try $ unsafeRunInOpenReadTransaction connFromVar qs0
                        PG.rollbackToAndReleaseSavepoint connFromVar sp
                        sendRsp rv
                  }
            pure (txidSnapshot, reader)
        }
  go dbDriver
