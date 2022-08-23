{-# LANGUAGE FlexibleContexts #-}
{-# Language RankNTypes #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-|
Description : Utility functions for creating worker threads, and Beam specific task workers for running SQL.
-}
module Rhyolite.Task.Beam.Worker where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Thread.Delay
import Control.Exception.Lifted (bracket)
import Control.Monad (forM)
import Control.Monad.Cont
import Control.Monad.Trans.Control
import Data.Time

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Schema.Tables

import Rhyolite.DB.Beam
import Rhyolite.Task.Beam
import Rhyolite.DB.Beam.Types (WrapColumnar(..))

-- | Takes a worker continuation and handles checking out and checking in a task
-- that is stored in a database table.  The 'Rhyolite.Task.Beam.Task' type tells
-- it how to find eligible tasks, how to extract a useful payload from the row,
-- and how to put results back into the row while the continuation does the real
-- work.
--
-- The worker continuation is divided into 3 phases:
--
--   1. A checkout action that is transaction safe (it may retry).
--   2. A work action that is not transaction safe (it will not retry).
--   3. A commit action that is transaction safe (it may retry).
--
-- The continuation can perform its own queries in the checkout transaction but
-- it is ideal to spend as little time as possible in this phase for the sake
-- of throughput.
taskWorker
  :: forall m be db table f payload checkout result.
     ( MonadIO m, Database be db, Beamable table, Table table
     , Beamable payload, Beamable result
     , FromBackendRow be (PrimaryKey table Identity)
     , FromBackendRow be (payload Identity)
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) result
     , HasSqlValueSyntax PgValueSyntax checkout
     , be ~ Postgres, f ~ QExpr Postgres (QNested QBaseScope)
     )
  => Connection
  -> DatabaseEntity be db (TableEntity table)
  -- ^ The table whose rows represent tasks to be run
  -> Task be table payload checkout result
  -- ^ Description of how task data is embedded within the table
  -> (PrimaryKey table Identity -> payload Identity -> Pg (m (Pg (result Identity))))
  -- ^ Worker continuation
  -> checkout
  -- ^ Identifier for the worker checking out the task
  -> m Bool
taskWorker dbConn table schema k checkoutId = do
  -- Checkout Phase
  mCheckout <-
    -- Do the following inside a transaction:
    -- 1. Get the first task that is not currently checked out by any worker
    -- 2. Update this task to reflect that it has been checked out by current worker
    -- 3. Run the specified checkout task which returns the work continuation
    withTransactionSerializableRunBeamPostgres dbConn $ do
      primaryKeyAndInput <- runSelectReturningOne $ select $ limit_ 1 $ do
          task <- all_ table

          -- Both task fields should be empty for an unclaimed task
          -- Also apply any other filters that may have been passed, using ready
          guard_ $ isNothing_ (_task_checkedOutBy schema task)
               &&. (_task_filter schema task)

          -- Return the primary key (task id) along with a custom field that the user asked for.
          pure (primaryKey task, _task_payload schema task)
      -- In case we did not find any rows, no update SQL will be run
      -- The row lock that we acquired above will be reset when the transaction ends.
      forM primaryKeyAndInput $ \(taskId, input) -> do
        -- Mark the retrieved task as checked out, by the current worker
        runUpdate $
          update table
            (\task -> _task_checkedOutBy schema task <-. val_ (Just checkoutId))
            (\task -> primaryKey task ==. val_ taskId)

        (,) taskId <$> k taskId input
  case mCheckout of
    Nothing -> pure False
    Just (taskId, workAction) -> do
      -- Work phase
      commitAction <- workAction

      -- Commit phase
      withTransactionSerializableRunBeamPostgres dbConn $ do
        -- Get the result value from the serializable
        b <- commitAction

        -- Update the task's result field, set checked out field to null
        runUpdate $ update table
          (\task -> mconcat
            [ _task_result schema task <-. val_ b
            , _task_checkedOutBy schema task <-. val_ Nothing
            ])
          (\task -> primaryKey task ==. val_ taskId)

      pure True


-- | Takes a worker continuation and handles checking out and checking in a task
-- that is stored in a database table.  The 'Rhyolite.Task.Beam.Task' type tells
-- it how to find eligible tasks, how to extract a useful payload from the row,
-- and how to put results back into the row while the continuation does the real
-- work.
--
-- The worker continuation is divided into 3 phases:
--
--   1. A checkout action that is transaction safe (it may retry).
--   2. A work action that is not transaction safe (it will not retry).
--   3. A commit action that is transaction safe (it may retry).
--
-- The continuation can perform its own queries in the checkout transaction but
-- it is ideal to spend as little time as possible in this phase for the sake
-- of throughput.
taskWorker1
  :: forall m be db table f payload checkout result.
     ( MonadIO m, Database be db, Beamable table, Table table
     , Beamable payload, Beamable result
     , FromBackendRow be (PrimaryKey table Identity)
     , FromBackendRow be (payload Identity)
     , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) result
     , HasSqlValueSyntax PgValueSyntax checkout
     , be ~ Postgres, f ~ QExpr Postgres (QNested QBaseScope)
     )
  => Connection
  -> DatabaseEntity be db (TableEntity table)
  -- ^ The table whose rows represent tasks to be run
  -> Task be table payload checkout result
  -- ^ Description of how task data is embedded within the table
  -> (forall x. table x -> C x Bool)
  -- ^ Which field indicates that the task result has been checked in.
  -> (PrimaryKey table Identity -> payload Identity -> Pg (m (Pg (result Identity))))
  -- ^ Worker continuation
  -> checkout
  -- ^ Identifier for the worker checking out the task
  -> m Bool
taskWorker1 dbConn table schema hasRun k = taskWorker dbConn table schema1 $ \tId p -> do
  k' <- k tId p
  pure $ do
    k'' <- k'
    pure $ do
      res <- k''
      pure (res :*: WrapColumnar True)
  where
    schema1 = schema
      { _task_filter = \tbl -> not_ (hasRun tbl) &&. _task_filter schema tbl
      , _task_result = \tbl -> _task_result schema tbl :*: WrapColumnar (hasRun tbl)
      }


-- | Run a worker thread
-- The worker will wake up whenever the timer expires or the wakeup action is called
-- Once woken up, the worker will be run repeatedly until it reports that it was not able to find any work to do; then it will start sleeping for the given duration.
-- If the wakeup action is called while the worker is running, the worker will run again as soon as it finishes, even if it returns False.  This is necessary because otherwise, since checking for work isn't generally atomic, there would be a race condition: worker starts (e.g. enters looking-for-work transaction), work is created, wakeup called, worker finishes with no work found, sleep.
withWorker
  :: (MonadIO m, MonadBaseControl IO m)
  => NominalDiffTime
  -- ^ Sleep interval
  -> IO Bool
  -- ^ The worker action, which reports whether it found work to do or not.
  -> (IO () -> m a)
  -- ^ Program that is given the option to prompt the worker thread to look for work.
  -> m a
withWorker d work child = do
  initialStartVar <- liftIO $ newMVar ()
  startVarVar <- liftIO $ newMVar initialStartVar
  let wakeup = void $ withMVar startVarVar $ \startVar -> tryPutMVar startVar ()
      sleep startVar = void $ forkIO $ do
        delay $ ceiling $ d * 1000000
        putMVar startVar () -- Do this blockingly so the thread can theoretically be GCed if it becomes useless
      go startVar = do
        nextStartVar <- newEmptyMVar
        void $ takeMVar startVar
        modifyMVar_ startVarVar $ \_ -> pure nextStartVar
        didWork <- withAsync work waitCatch
        case didWork of
          Left e -> do
            putStrLn $ "withWorker: error in worker: " <> show e --TODO: Use MonadLogger
            sleep nextStartVar
          Right True -> void $ tryPutMVar nextStartVar ()
          Right False -> sleep nextStartVar
        go nextStartVar
  bracket (liftIO $ async $ go initialStartVar) (liftIO . cancel) $ \_ -> child wakeup

-- | Run multiple workers in parallel on the same task
withConcurrentWorkers
  :: forall m r a. (MonadIO m, MonadBaseControl IO m)
  => Int
     -- ^ Number of workers
  -> NominalDiffTime
     -- ^ Sleep interval for each worker
  -> (a -> IO Bool)
     -- ^ Parameterized worker action
  -> (Int -> a)
     -- ^ How to derive the worker's action parameter from its index
  -> (IO () -> m r)
     -- ^ Program that is given the option to prompt all the worker threads to look for work.
  -> m r
withConcurrentWorkers n0 d work argFn = runContT $ go n0
  where
    go :: Int -> ContT r m (IO ())
    go n = do
      wakeup <- ContT $ withWorker d (work $ argFn n)
      wakeupRest <- if n > 1
        then go (n-1)
        else return $ return ()
      return (wakeup *> wakeupRest)
