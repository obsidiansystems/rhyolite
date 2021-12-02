{-# Language FlexibleContexts #-}
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
import Control.Lens ((^.), Lens')
import Control.Monad (forM)
import Control.Monad.Cont
import Control.Monad.Logger (askLoggerIO, MonadLoggerIO)
import Control.Monad.Logger.Extras (Logger(..))
import Control.Monad.Trans.Control
import Data.Time

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Schema.Tables

import Database.PostgreSQL.Serializable
import Rhyolite.DB.Beam
import Rhyolite.Task.Beam

-- | This function retrieves the first unclaimed task from the table (passed as an argument).
-- It then assigns this task to itself, and runs the action passed to it.
-- Once it has the output of that action, it sets the output as the result of the task,
-- thereby marking it as completed.
taskWorker ::
  forall m be db f table a b c.
  ( MonadIO m, MonadLoggerIO m, Database be db
  , Beamable table, Table table
  , FromBackendRow be (PrimaryKey table Identity)
  , FieldsFulfillConstraint (HasSqlEqualityCheck be) (PrimaryKey table)
  , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey table)
  , FromBackendRow be a, HasSqlValueSyntax PgValueSyntax b, HasSqlValueSyntax PgValueSyntax c
  , be ~ Postgres, f ~ QExpr be (QNested QBaseScope))
  => Connection -- ^ Connection to the database
  -> DatabaseEntity be db (TableEntity table) -- ^ A table containing embedded tasks.
  -> f Bool -- ^ A filter for selecting tasks from the table.
  -> (forall x. Lens' (table x) (Task a b c x)) -- ^ Lens for retrieving a task from the table.
  -> (a -> Serializable (m (Serializable b))) -- ^ The action, whose output is set as the result of the task.
  -> c -- ^ Id of the worker
  -> m Bool
taskWorker dbConn table ready field go workerId = do
  logger <- askLoggerIO
  checkedOutValue <-
    -- BEGIN Transaction

    -- Do the following inside a transaction:
    -- 1. Get the first task that is not currently checked out by any worker, lock this task
    -- 2. Update this task to reflect that it has been checked out by current worker
    -- 3. Perform the serializable task
    withTransactionSerializableRunBeamPostgres dbConn $ do
      primaryKeyAndInput <- runSelectReturningOne $ select $ limit_ 1 $ do
          task <- all_ table

          -- Both task fields should be empty for an unclaimed task
          -- Also apply any other filters that may have been passed, using ready
          guard_ $ isNothing_ (task ^. field . taskResult) &&. isNothing_ (task ^. field . taskCheckedOutBy) &&. ready

          -- Return the primary key (task id) along with a custom field that the user asked for.
          pure (primaryKey task, task ^. field . taskPayload)

      -- In case we did not find any rows, no update SQL will be run
      -- The row lock that we acquired above will be reset when the transaction ends.
      forM primaryKeyAndInput $ \(taskId, input) -> do
        -- Mark the retrieved task as checked out, by the current worker
        runUpdate $
          update table
            (\task -> (task ^. field . taskCheckedOutBy) <-. val_ (Just workerId))
            (\task -> primaryKey task ==. val_ taskId)

        runSerializableInsideTransaction dbConn (Logger logger) $ (,) taskId <$> go input
    -- END Transaction, release row lock

  case checkedOutValue of
    Nothing -> pure False
    Just (taskId, action) -> do
      -- Get the followup Serializable
      followup <- action

      -- BEGIN Transaction
      withTransactionSerializableRunBeamPostgres dbConn $ do
        -- Get the result value from the serializable
        b <- runSerializableInsideTransaction dbConn (Logger logger) followup

        -- Update the task's result field, set checked out field to null
        runUpdate $ update table
          (\task -> mconcat
            [ task ^. field . taskResult <-. val_ (Just b)
            , task ^. field . taskCheckedOutBy <-. val_ Nothing])
          (\task -> primaryKey task ==. val_ taskId)
      -- END Transaction

      pure True

-- | Run a worker thread
-- The worker will wake up whenever the timer expires or the wakeup action is called
-- Once woken up, the worker will be run repeatedly until it reports that it was not able to find any work to do; then it will start sleeping for the given duration.
-- If the wakeup action is called while the worker is running, the worker will run again as soon as it finishes, even if it returns False.  This is necessary because otherwise, since checking for work isn't generally atomic, there would be a race condition: worker starts (e.g. enters looking-for-work transaction), work is created, wakeup called, worker finishes with no work found, sleep.
withWorker :: (MonadIO m, MonadBaseControl IO m) => NominalDiffTime -> IO Bool -> (IO () -> m a) -> m a
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
  :: forall r a. Int
  -> NominalDiffTime
  -> (a -> IO Bool)
  -> (Int -> a)
  -> (IO () -> IO r)
  -> IO r
withConcurrentWorkers n0 d work argFn = runContT $ go n0
  where
    go :: Int -> ContT r IO (IO ())
    go n = do
      wakeup <- ContT $ withWorker d (work $ argFn n)
      wakeupRest <- if n > 1
        then go (n-1)
        else return $ return ()
      return (wakeup *> wakeupRest)
