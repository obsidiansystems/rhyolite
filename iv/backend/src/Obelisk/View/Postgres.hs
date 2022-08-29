--TODO: This module now has almost nothing to do with Postgres
{-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Obelisk.View.Postgres
  ( runDbIvSequential
  , runDbIv
  , DbDriver (..)
  , DbReader (..)
  --TODO: Move out
  , MonadLog (..)
  , tshow
  , AsyncReadDb (..)
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
import Obelisk.Beam.Patch.Decode.Postgres
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



closeAllInRef
  :: ( MonadRef m
     , MonadLog m
     , RefData m (IntMap (DbReader db m))
     )
  => (DbReader db m -> m ())
  -> Ref m (IntMap (DbReader db m))
  -> m ()
closeAllInRef closeReader r = do
  cs <- readRef r
  putLog $ "closing " <> tshow (IntMap.size cs) <> " connections"
  mapM_ closeReader cs

killAllThreads
  :: ( MonadAtomicRef m
     , MonadLog m
     , MonadFork m
     , RefData m (Set (ThreadId m))
     )
  => Ref m (Set (ThreadId m))
  -> m ()
killAllThreads r = do
  threads <- atomicModifyRef' r $ \s -> (Set.empty, s)
  putLog $ "killing " <> tshow (Set.size threads) <> " threads"
  mapM_ killThread threads

--TODO: I think there may be some possible inconsistency: logical decoding steps forward by LSN, but new transaction snapshots do not necessarily match any particular LSN.  See https://www.postgresql.org/message-id/CA%2BTgmoZkGbgcu18vr4ewx6L9n8Nfe89GADc2W7kwhdWa8YRh%3Dg%40mail.gmail.com

data AsyncReadDb m = forall x. AsyncReadDb (ReadDb x) (x -> m ())

runDbIvSequential
  :: forall db a b m
  .  ( Monad m
     , DecodableDatabase db
     , HasT IsTable db
     , DZippable db
     -- , HasT (TableHas Eq (ComposeMaybe (Compose (Const ()) TablePatch))) db
     -- , HasT (TableHas Coverage (Compose (Const ()) TablePatch)) db
     -- , HasT (TableHas FullCoverage (Compose (Const ()) TablePatch)) db
     -- , HasT (TableHas HasCov (Compose Identity TablePatch)) db
     -- , HasT (TableHas Coverable (Compose Identity TablePatch)) db
     -- , _
     , RefData m (Set (ThreadId m))
     , MVarData m ()
     , MVarData m (Int -> QueryResultPatch (TablesV db) TablePatch -> m ())
     , ConstraintsForT db (TableHas_ (ComposeC Semigroup TablePatch))
     , ConstraintsForT db (TableHas_ EmptyConstraint)
     , RefData m (IntMap (DbReader db m))
     , MonadConc m
     , MonadFail m
     , MonadAtomicRef m
     , MonadLog m
     , MonadMVar m
     , MonadFork m
     , DPointed db
     , Ord (ThreadId m)
     , Show (db (TableOnly (ComposeMaybe TablePatchInfo)))
     )
  => DbDriver db m
  -> ((NonEmptyInterval -> m ())
      -> (Time -> AsyncReadDb m -> m ())
      -> m (Time -> QueryResultPatch (TablesV db) TablePatch -> m (), b))
  -> (Time -> m ())
  -> (b -> m a)
  -> m a
runDbIvSequential dbDriver = runDbIv dbDriver 1

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
     , MonadLog m
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
  => DbDriver db m
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
runDbIv (DbDriver withFeed openReader) initialTime startMyIv setTime go = do
  transactions <- atomically newTChan
  let enqueueTransaction txn = do
        atomically $ writeTChan transactions $ Right txn
      releaseSnapshot s = do
        atomically $ writeTChan transactions $ Left s
  withFeed enqueueTransaction releaseSnapshot $ do
    bracket (newRef mempty) (closeAllInRef _dbReader_close) $ \openReadTransactionsRef -> do
      bracket (newRef mempty) killAllThreads $ \allWorkersRef -> do
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
          withAsync (walkTransactionLog openReader initialTime transactions onInitialReaderReady onSubsequentReaderReady) $ \_ -> do
            takeMVar readyToStartVar
            (sendPatch, bOut) <- startMyIv closeTime readAtTime
            putMVar sendPatchVar sendPatch
            go bOut

-- | Given a way of opening read transactions, a starting time, and stream of transactions, call the given callbacks in such a way as to "walk" through the transaction log, processing contiguous blocks of transactions and providing open transactions at each of the points betweeen those blocks.
walkTransactionLog
  :: forall m db
  .  (MonadLog m, MonadConc m)
  => m (TxidSnapshot, DbReader db m) -- ^ The command to open a reader
  -> Time -- ^ The initial logical time; the walker will consider its initial reader to be at this time, and all subsequent readers to be at subsequent times.  The actual magnitude is not important, and typically `1` will be passed here.
  -> TChan (STM m) (Either TxidSnapshot (Xid, QueryResultPatch (TablesV db) TablePatch)) -- ^ The channel from which the walker can retrieve transactions and snapshot fences.  This is expected to come from a logical decoding slot on the upstream server.
  -> (DbReader db m -> m ()) -- ^ The walker will call this when the initial reader is opened; there's no patch associated with the initial reader.
  -> (Seq (QueryResultPatch (TablesV db) TablePatch) -> Time -> DbReader db m -> m ()) -- ^ The walker will call this when any reader after the first is ready.  The given patch will be the patch between the prior reader (i.e. reader whose time is the predecessor of this one) and this one.
  -> m Void
walkTransactionLog openReader initialTime transactions initialReaderReady subsequentReaderReady = do
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


-- -- | Buffer notifications until their subscription state is resolved
-- --
-- -- In particular, this gadget forwards subscriptions properly, but allows
-- -- notifications to occur before that state is known, and silently discards
-- -- unwanted notifications.
-- startIv
--   :: forall a b m
--   .  ( Monad m
--      , MonadAtomicRef m
--      , MonadLog m
--      , MonadFix m
--      , MonadFork m
--      , Coverable (Pull a)
--      , Coverable (Push a)
--      , Coverage (Cov (Pull a))
--      , Coverage (Cov (Push a))
--      , Coverage (Cov (Pull b))
--      , Coverage (Cov (Push b))
--      -- , RefData m (CoverageMap (WithFullCoverage (Cov (Pull a))))
--      -- , RefData m (CoverageMap (WithFullCoverage (Cov (Pull b))))
--      -- , RefData m (CoverageMap (WithFullCoverage (Cov (Push b))))
--      -- , RefData m (BufferPushState (Push a))
--      , MonadMVar m
--      , Collidable (Push a)
--      , Show (Cov (Push a))
--      , Show (WithFullCoverage (Cov (Push a)))
--      , Show (Collision (Push a))
--      , _
--      )
--   => Iv m a b -- ^ The Iv to run
--   -> IvForward m b -- ^ Where to send the Iv's output
--   -> (NonEmptyInterval -> m ()) -- ^ Notify the data source that we're done with the Times in the given range
--   -> (Time -> Cov (Pull a) -> (Pull a -> m ()) -> m ()) -- Request some stuff at a particular Time
--   -> m ( Time -> Push a -> m () -- Push a value through the Iv; this should be called on each Time exactly once
--        , IvBackward m b
--        )
-- startIv iv fIn closeTime readAtTime = do
--   -- The amount of the input coverage space that has been covered.  When
--   -- a given index reaches 100% coverage, the associated read
--   -- transaction will be closed and the connection will be returned to
--   -- the pool.
--   inputReadCoverageRef :: Ref m (CoverageMap (WithFullCoverage (Cov (Pull a)))) <- newRef emptyCoverageMap
-- 
--   let -- Note that reads are done for a given region, either because
--       -- they've already been performed or because we've been informed
--       -- that they won't be needed.  When an index of the region
--       -- corresponding with a transaction is fully done, close the
--       -- transaction and return its connection to the pool.
--       finishReadCoverage q = do
--         coverageToClose <- atomicModifyRef' inputReadCoverageRef $ \old ->
--           let new = old `unionCoverageMaps` q
--               justFullCoverage c = if c == fullCoverage then Just () else Nothing
--           in ( new --TODO: Error on overlap
--              , mapCoverageMapMaybe justFullCoverage new `differenceCoverageMaps` mapCoverageMapMaybe justFullCoverage old
--              )
--         flip traverseWithInterval_CoverageMap coverageToClose $ \i _ -> do
--           putLog $ "readDone " <> tshow i
--           closeTime i
--   rec
--       (fOut, bIn) <- bufferPush fOut' $ IvBackward
--             { _ivBackward_read = \req t -> do
--                 readAtTime t req $ \rsp -> void $ fork $ do
--                   _ivForward_readResponse fOut rsp t
--                   finishReadCoverage $ singletonCoverageMap t $ toWithFullCoverage $ covered rsp -- This is a bit awkward; instead of calling `closeTime` when we've *received* all the queries we wanted, we should probably call it when we've *sent* them all; that potentially would let the postgres side, e.g., enqueue the `commit` command after the final query without needing to wait for a roundtrip
--             , _ivBackward_readNone = finishReadCoverage
--             , _ivBackward_subscribe = \_ _ -> pure ()
--             , _ivBackward_unsubscribe = \_ _ -> pure ()
--             , _ivBackward_subscribeNone = \_ -> pure () --TODO: Do we need to do anything with this information?  Yes: we shouldn't send data through when it hasn't been requested.  However, we can't avoid *accumulating* transaction info, because otherwise we could end up pulling from a state without having the ability to follow up by pushing.
--             }
-- 
--       (fOut', bOut) <- unIv iv fIn (bIn :: IvBackward m a)
-- 
-- 
--   readsSent <- newRef emptyCoverageMap
--   subscribesSent <- newRef emptyCoverageMap
-- 
--   let bOutWithMonitor = IvBackward --TODO: This monitoring should be unnecessary now that we've moved the finalization logic to makeSequential
--         { _ivBackward_read = \q t -> do
--             atomicModifyRef' readsSent $ \old ->
--               assert (old `intersectionCoverageMaps` singletonCoverageMap t (toWithFullCoverage q) == emptyCoverageMap) $
--               ( old `unionCoverageMapsAssertDisjoint` singletonCoverageMap t (toWithFullCoverage q)
--               , ()
--               )
--             _ivBackward_read bOut q t
--         , _ivBackward_readNone = \c -> do
--             -- Note: this should only happen when we're cleaning up a Time
--             atomicModifyRef' readsSent $ \old ->
--               assert (old `intersectionCoverageMaps` c == emptyCoverageMap) $
--               ( old `unionCoverageMapsAssertDisjoint` c
--               , ()
--               )
--             _ivBackward_readNone bOut c
--         , _ivBackward_subscribe = \q t -> do
--             atomicModifyRef' subscribesSent $ \old ->
--               assert (old `intersectionCoverageMaps` singletonCoverageMap t (toWithFullCoverage q) == emptyCoverageMap) $
--               ( old `unionCoverageMapsAssertDisjoint` singletonCoverageMap t (toWithFullCoverage q)
--               , ()
--               )
--             _ivBackward_subscribe bOut q t
--         , _ivBackward_unsubscribe = \q t -> do
--             atomicModifyRef' subscribesSent $ \old ->
--               assert (old `intersectionCoverageMaps` singletonCoverageMap t (toWithFullCoverage q) == emptyCoverageMap) $
--               ( old `unionCoverageMapsAssertDisjoint` singletonCoverageMap t (toWithFullCoverage q)
--               , ()
--               )
--             _ivBackward_unsubscribe bOut q t
--         , _ivBackward_subscribeNone = \c -> do
--             -- Note: this should only happen when we're cleaning up a Time
--             atomicModifyRef' subscribesSent $ \old ->
--               assert (old `intersectionCoverageMaps` c == emptyCoverageMap) $
--               ( old `unionCoverageMapsAssertDisjoint` c
--               , ()
--               )
--             _ivBackward_subscribeNone bOut c
--         }
--       sendPatch n filteredPatch = do
--         putLog $ "Sending notification for transaction " <> tshow n
--         _ivForward_notify fOut filteredPatch n
--         putLog $ "Finalizing notification for transaction " <> tshow n
--         traverse_ (_ivForward_notifyNone fOut . singletonCoverageMap n) $ fullCoverage `differenceCoverage` toWithFullCoverage (covered filteredPatch)
--         putLog $ "Done with transaction " <> tshow n
--   pure (sendPatch, bOutWithMonitor)

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

class MonadLog m where
  putLog :: Text -> m ()

instance MonadLog IO where
  putLog x = do
    tid <- myThreadId
    t <- getCurrentTime
    T.putStrLn $ tshow t <> " " <> tshow tid <> ": " <> x

tshow :: Show a => a -> Text
tshow = T.pack . show

--TODO: Connection 1 seems not to close
