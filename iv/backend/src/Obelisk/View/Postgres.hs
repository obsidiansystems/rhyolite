--TODO: This module now has almost nothing to do with Postgres
{-# LANGUAGE PartialTypeSignatures #-}

module Obelisk.View.Postgres
  ( runDbIv
  , DbDriver (..)
  , DbReader (..)
  --TODO: Move out
  , tshow
  , AsyncReadDb (..)
  , TablePatchInfo
  ) where

import Prelude hiding (id, (.), fail)

import Control.Category
import Control.Concurrent.Classy (atomically, MonadConc)
import Control.Concurrent.Classy.Async
import Control.Concurrent.Classy.STM
import Control.Monad hiding (fail)
import Control.Monad.Catch
import Control.Monad.Conc.Class (STM)
import Control.Monad.Fail
import Control.Monad.MVar.Restricted
import Control.Monad.Ref.Restricted
import Data.Constraint.Compose
import Data.Constraint.Empty
import Data.Foldable
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock
import Data.Void
import Obelisk.Beam.Constraints
import Obelisk.Beam.DZippable
import Obelisk.Beam.Patch.Db
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding (Xid)
import Obelisk.Postgres.Snapshot
import Obelisk.View.NonEmptyInterval
import Obelisk.View.Time
import qualified Control.Concurrent
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Patch.MapWithPatchingMove
import Obelisk.Api
import Obelisk.View.Orphans ()

sliceIntMap :: NonEmptyInterval -> IntMap v -> (IntMap v, IntMap v, IntMap v)
sliceIntMap (NonEmptyInterval start end) m =
  let (beforeStart, atStart, afterStart) = IntMap.splitLookup start m
      (withinInterval, atEnd, afterEnd) = IntMap.splitLookup end afterStart
  in ( beforeStart
     , maybe id (IntMap.insertWith (error "duplicate start key") start) atStart $
       maybe id (IntMap.insertWith (error "duplicate end key") end) atEnd $
       withinInterval
     , afterEnd
     )


type Logger m = Text -> m ()

closeAllInRef
  :: ( MonadRef m
     , RefData m (IntMap (DbReader db m))
     )
  => Logger m
  -> (DbReader db m -> m ())
  -> Ref m (IntMap (DbReader db m))
  -> m ()
closeAllInRef putLog closeReader r = do
  cs <- readRef r
  putLog $ "closing " <> tshow (IntMap.size cs) <> " connections"
  mapM_ closeReader cs

killAllThreads
  :: ( MonadAtomicRef m
     , MonadFork m
     , RefData m (Set (ThreadId m))
     )
  => Logger m
  -> Ref m (Set (ThreadId m))
  -> m ()
killAllThreads putLog r = do
  threads <- atomicModifyRef' r $ \s -> (Set.empty, s)
  putLog $ "killing " <> tshow (Set.size threads) <> " threads"
  mapM_ killThread threads

--TODO: I think there may be some possible inconsistency: logical decoding steps forward by LSN, but new transaction snapshots do not necessarily match any particular LSN.  See https://www.postgresql.org/message-id/CA%2BTgmoZkGbgcu18vr4ewx6L9n8Nfe89GADc2W7kwhdWa8YRh%3Dg%40mail.gmail.com

data AsyncReadDb m = forall x. AsyncReadDb (ReadDb x) (x -> m ())

-- | Analogous to a read-only transaction
data DbReader db m = DbReader
  { _dbReader_getVisibleTransactionsSince :: TxidSnapshot -> TxidSnapshot -> m (Set Xid)
  , _dbReader_close :: m ()
  , _dbReader_runRead :: AsyncReadDb m -> m ()
  }

data DbDriver db m = DbDriver
  { _dbDriver_withFeed :: forall a. ((Xid, QueryResultPatch (TablesV db) TablePatch) -> m ()) -> (TxidSnapshot -> m ()) -> m a -> m a
  , _dbDriver_openReader :: m (TxidSnapshot, DbReader db m)
  }

data TablePatchInfo x = TablePatchInfo
  { _tablePatchInfo_inserts :: Int
  , _tablePatchInfo_updates :: Int
  , _tablePatchInfo_deletes :: Int
  } deriving Show

instance Semigroup (TablePatchInfo x) where
  TablePatchInfo x y z <> TablePatchInfo x' y' z' = TablePatchInfo (x + x') (y + y') (z + z')

tablePatchInfo
  :: (DMappable db)
  => QueryResultPatch db TablePatch -> Maybe (QueryResultPatch db TablePatchInfo)
tablePatchInfo = mapQueryResultPatchMaybe $ \(TablePatch (PatchMapWithPatchingMove x)) -> foldMap (\(NodeInfo f t) ->
  case f of
    From_Insert _ -> Just $ TablePatchInfo 1 0 0
    From_Delete -> Just $ TablePatchInfo 0 0 (maybe 1 (const 0) t)
    From_Move _ _ -> Just $ TablePatchInfo 0 1 0
  ) x


--TODO: Keep track of *all* threads so they can be killed when we exit
runDbIv
  :: forall db a m b
  .  ( MonadConc m
     , MonadFail m
     , MonadAtomicRef m
     , MonadMVar m
     , RefData m (Set (ThreadId m))
     , MonadFork m
     , MVarData m ()
     , MVarData m (Int -> QueryResultPatch (TablesV db) TablePatch -> m ())
     , HasT (TableHas_ (ComposeC Semigroup TablePatch)) db
     , HasT (TableHas_ EmptyConstraint) db
     , Ord (ThreadId m)
     , RefData m (IntMap (DbReader db m))
     , HasT IsTable db
     -- , DZippable db
     -- , Show (db (TableOnly (ComposeMaybe TablePatch)))
     , Show (db (TableOnly (ComposeMaybe TablePatchInfo)))
     , DPointed db
     -- , Control.Monad.Conc.Class.STM m ~ GHC.Conc.Sync.STM
     )
  => Logger m
  -> DbDriver db m
  -> Time
  -> ( (NonEmptyInterval -> m ())
      -> (Time -> AsyncReadDb m -> m ())
      -> m ( Time -> QueryResultPatch (TablesV db) TablePatch -> m ()
           , b
           )
     )
  -> (Time -> m ())
  -> (b -> m a)
  -> m a
runDbIv putLog (DbDriver withFeed openReader) initialTime startMyIv setTime go = do
  transactions <- atomically newTChan
  let enqueueTransaction txn = do
        atomically $ writeTChan transactions $ Right txn
      releaseSnapshot s = do
        atomically $ writeTChan transactions $ Left s
  withFeed enqueueTransaction releaseSnapshot $ do
    bracket (newRef mempty) (closeAllInRef putLog _dbReader_close) $ \openReadTransactionsRef -> do
      bracket (newRef mempty) (killAllThreads putLog) $ \allWorkersRef -> do
        let forkWorker :: m () -> m ()
            forkWorker a = void $ fork $ do
              tid <- myThreadId
              let add = atomicModifyRef' allWorkersRef $ \w -> (Set.insert tid w, ())
                  remove = atomicModifyRef' allWorkersRef $ \w -> (Set.delete tid w, ())
              bracket_ add remove a
        handle (\(SomeException e) -> putLog (tshow e) >> throwM e) $ do
          let closeTime i = do
                (toClose, stale) <- atomicModifyRef' openReadTransactionsRef $ \conns ->
                  let (before, toClose, after) = sliceIntMap i conns
                  in ( before <> after
                     , (toClose, IntMap.keys before )
                     )
                putLog $ "closing connections: " <> tshow (IntMap.keys toClose)
                unless (null stale) $ putLog $ "stale connections:" <> tshow stale
                forM_ toClose $ \conn -> do
                  fork $ do --We want to close all transactions ASAP, even if some are long-running
                    _dbReader_close conn
              readAtTime t req = void $ fork $ do
                openReadTransactions <- readRef openReadTransactionsRef
                case IntMap.lookup t openReadTransactions of
                  Nothing -> fail $ "Expected transaction " <> show t <> " to be open"
                  Just conn -> _dbReader_runRead conn req

          readyToStartVar <- newEmptyMVar
          sendPatchVar <- newEmptyMVar
          let addReader t reader = do
                atomicModifyRef' openReadTransactionsRef $ \old ->
                  ( IntMap.insertWith (error $ "duplicate time " <> show t) t reader old
                  , ()
                  )
                putLog $ "Inserted reader for time " <> tshow t
              onInitialReaderReady reader = do
                addReader initialTime reader
                putMVar readyToStartVar ()
              onSubsequentReaderReady patches t reader = do
                addReader t reader
                setTime t
                let p = mconcat $ reverse $ toList patches
                putLog $ "sendPatch " <> tshow (pred t) <> " " <> tshow (tablePatchInfo p)
                withMVar sendPatchVar $ \sendPatch -> do -- We don't really need to hold this mutex, we just can't send the first one before we've got it
                  forkWorker $ sendPatch (pred t) p
          withAsync (walkTransactionLog putLog openReader initialTime transactions onInitialReaderReady onSubsequentReaderReady) $ \_ -> do
            takeMVar readyToStartVar
            (sendPatch, bOut) <- startMyIv closeTime readAtTime
            putMVar sendPatchVar sendPatch
            go bOut

-- | Given a way of opening read transactions, a starting time, and stream of transactions, call the given callbacks in such a way as to "walk" through the transaction log, processing contiguous blocks of transactions and providing open transactions at each of the points betweeen those blocks.
walkTransactionLog
  :: forall m db
  .  (MonadConc m)
  => Logger m
  -> m (TxidSnapshot, DbReader db m) -- ^ The command to open a reader
  -> Time -- ^ The initial logical time; the walker will consider its initial reader to be at this time, and all subsequent readers to be at subsequent times.  The actual magnitude is not important, and typically `1` will be passed here.
  -> TChan (STM m) (Either TxidSnapshot (Xid, QueryResultPatch (TablesV db) TablePatch)) -- ^ The channel from which the walker can retrieve transactions and snapshot fences.  This is expected to come from a logical decoding slot on the upstream server.
  -> (DbReader db m -> m ()) -- ^ The walker will call this when the initial reader is opened; there's no patch associated with the initial reader.
  -> (Seq (QueryResultPatch (TablesV db) TablePatch) -> Time -> DbReader db m -> m ()) -- ^ The walker will call this when any reader after the first is ready.  The given patch will be the patch between the prior reader (i.e. reader whose time is the predecessor of this one) and this one.
  -> m Void
walkTransactionLog putLog openReader initialTime transactions initialReaderReady subsequentReaderReady = do
  let burnOldTransactions :: TxidSnapshot -> m ()
      burnOldTransactions snapshot = do
        atomically (readTChan transactions) >>= \case
          Left _ -> burnOldTransactions snapshot -- Just ignore fences
          Right txn -> do
            putLog $ "burning transaction: " <> tshow (fst txn)
            if fst txn `transactionVisibleInSnapshot` snapshot
              then burnOldTransactions snapshot
              else atomically $ unGetTChan transactions $ Right txn --TODO: Do something more elegant than unGetTChan
      stepTransaction :: TxidSnapshot -> Set Xid -> Seq (QueryResultPatch (TablesV db) TablePatch) -> m (Seq (QueryResultPatch (TablesV db) TablePatch), TxidSnapshot, DbReader db m)
      stepTransaction lastSnapshot alreadyReceivedXids alreadyReceivedTransactions = do
        putLog "Opening reader"
        (snapshot, reader) <- openReader
        putLog "Reader opened"
        -- Get the Xids we need to wait for
        xidsSinceLastSnapshot <- _dbReader_getVisibleTransactionsSince reader lastSnapshot snapshot
        let collectContiguousXids :: Set Xid -> Set Xid -> Seq (QueryResultPatch (TablesV db) TablePatch) -> m (Seq (QueryResultPatch (TablesV db) TablePatch), TxidSnapshot, DbReader db m)
            collectContiguousXids pendingXids receivedXids receivedTransactions =
              if Set.null pendingXids then pure (receivedTransactions, snapshot, reader) else do
                -- putLog "Waiting for replication data"
                atomically (readTChan transactions) >>= \case
                  Left s -> do
                    let (finishedXids, remainingXids) = Set.partition (`transactionVisibleInSnapshot` s) pendingXids
                    unless (Set.null finishedXids) $ putLog $ "Fence discharged " <> tshow (Set.toList finishedXids) <> ", leaving " <> tshow (Set.size remainingXids)
                    collectContiguousXids remainingXids (receivedXids `Set.union` finishedXids) receivedTransactions
                  Right (xid, transaction) -> do
                    -- putLog $ "Got transaction " <> tshow xid <> " while waiting for transactions " <>
                    --   if Set.size pendingXids > 25
                    --     then "#" <> tshow (Set.size pendingXids)
                    --     else tshow (toList pendingXids)
                    case xid `Set.member` pendingXids of
                      True -> do
                        collectContiguousXids (Set.delete xid pendingXids) (Set.insert xid receivedXids) (receivedTransactions <> Seq.singleton transaction) --TODO: Is there any reason to retain the transactions here, or should we just mappend the transactions right away?
                      -- We received a transaction that we weren't waiting for, so we need to bail out and try to find a new transaction
                      False -> do
                        putLog "Non-contiguous transactions: trying again with another reader"
                        _dbReader_close reader
                        stepTransaction lastSnapshot (Set.insert xid receivedXids) (receivedTransactions <> Seq.singleton transaction)
        collectContiguousXids (xidsSinceLastSnapshot `Set.difference` alreadyReceivedXids) alreadyReceivedXids alreadyReceivedTransactions
  putLog $ "Opening reader for time " <> tshow initialTime
  (initialSnapshot, initialReader) <- openReader
  putLog $ "Reader " <> tshow initialTime <> " opened at snapshot " <> tshow initialSnapshot
  initialReaderReady initialReader
  burnOldTransactions initialSnapshot --TODO: Instead, when creating the replication slot, use EXPORT_SNAPSHOT, and then use that snapshot to create this transaction; otherwise, we might end up with the write transactions visible from this reader not corresponding to a contiguous ordering of transactions, which we can't easily detect or recover from
  let loop !t lastSnapshot = do
        --TODO: Get rid of this hack
        let waitForTransaction = do
              fence <- atomically $ do -- Wait until at least one transaction has been received; throw away any fences we've received in the mean time (they are redundant)
                readTChan transactions >>= \case
                  Left f -> do
                    pure $ Just f
                  Right txn -> do
                    unGetTChan transactions $ Right txn
                    pure Nothing
              case fence of
                Nothing -> pure ()
                Just f -> do
                  putLog $ "Discarding fence: " <> tshow f
                  waitForTransaction
        waitForTransaction
        (patches, snapshot, reader) <- stepTransaction lastSnapshot mempty mempty
        subsequentReaderReady patches t reader
        loop (succ t) snapshot
  loop (succ initialTime) initialSnapshot


--TODO: We can actually fully enforce foreign keys with phantom types, since our transactions are read-only

--TODO: In client views, tell them something about whether the view includes each of the write requests they've made

--TODO: Deal with Xid wraparound

--TODO: Ensure all atomicModifyIORefs are strict

--TODO: Switch to using raw Logical Replication rather than Logical Decoding (looks pretty much the same, but with a more efficient format)

--TODO: Get rid of all asserts and other partiality
--TODO: Should we have a separate makeSequential for each websocket?

--TODO:
-- - Clean up internal datastructures
--   - Probably need a way to bubble up who needs cleanup
-- - Performance
--   - Separate booting Ivs from running them - this way the important functions can all get inlined together
--   - Convert everything to monoidal instead of callback-driven

class MonadFork m where
  type ThreadId m
  fork :: m () -> m (ThreadId m)
  myThreadId :: m (ThreadId m)
  killThread :: ThreadId m -> m ()

instance MonadFork IO where
  type ThreadId IO = Control.Concurrent.ThreadId
  fork = Control.Concurrent.forkIO
  myThreadId = Control.Concurrent.myThreadId
  killThread = Control.Concurrent.killThread


tshow :: Show a => a -> Text
tshow = T.pack . show

--TODO: Connection 1 seems not to close
