{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Rhyolite.Backend.TaskWorker where

import Rhyolite.Backend.Schema.Task

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Thread.Delay
import Control.Exception.Lifted (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Functor.Identity
import Data.Pool
import Data.Text (Text)
import Data.Time
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Postgresql
import Rhyolite.Backend.DB

--TODO: Ensure Tasks are always properly indexed
--TODO: Use Notifications to start the worker promptly
--TODO: Use a single `update returning` to take things, or consider using postgres locking
--TODO: Alert when jobs are failing
--TODO: Recover from failed jobs somehow
--TODO: Run more than one worker at a time sometimes
--TODO: Timeout workers

taskWorker
  :: forall m v c p b k a
  .  ( MonadLogger m
     , MonadIO m
     , MonadBaseControl IO m
     , Projection p a
     , ProjectionDb p Postgresql
     , ProjectionRestriction p (RestrictionHolder v c)
     , EntityConstr v c
     , PrimitivePersistField b
     , SinglePersistField b
     , NeverNull b
     , Unifiable (SubField Postgresql v c (Maybe b)) (Maybe b)
     , Unifiable (k (UniqueMarker v)) (Key v (Unique k))
     , IsUniqueKey (Key v (Unique k))
     )
  => p
  -> k (UniqueMarker v)
  -> Field v c (Task b)
  -> (a -> m b)
  -> Pool Postgresql
  -> Text
  -> m Bool
taskWorker p pk f go db workerName = do
  checkedOutValue <- runDb (Identity db) $ do
    qe <- project1 (pk, p) $ isFieldNothing (f ~> Task_resultSelector) &&. isFieldNothing (f ~> Task_checkedOutBySelector)
    forM_ (fst <$> qe) $ \taskId ->
      update [f ~> Task_checkedOutBySelector =. Just workerName] $ pk ==. taskId
    return qe
  case checkedOutValue of
    Nothing -> pure False
    Just (taskId, a) -> do
      b <- go a
      Rhyolite.Backend.DB.runDb (Identity db) $ do
        update
          [ f ~> Task_resultSelector =. Just b
          , f ~> Task_checkedOutBySelector =. (Nothing :: Maybe Text)
          ]
          (pk ==. taskId)
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
        modifyMVar_ startVarVar $ \_ -> pure nextStartVar
        void $ takeMVar startVar
        didWork <- withAsync work waitCatch
        case didWork of
          Left e -> do
            putStrLn $ "withWorker: error in worker: " <> show e --TODO: Use MonadLogger
            sleep startVar
          Right True -> void $ tryPutMVar startVar ()
          Right False -> sleep startVar
        go nextStartVar
  bracket (liftIO $ async $ go initialStartVar) (liftIO . cancel) $ \_ -> child wakeup
