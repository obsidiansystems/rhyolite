{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Rhyolite.Backend.TaskWorker where

import Rhyolite.Backend.Schema.Task

import Control.Monad.Cont
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Thread.Delay
import Control.Exception.Lifted (bracket, catch, throw, Exception, SomeException(..))
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Functor.Identity
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Groundhog.Core
import Database.Groundhog.Expression
import Database.Groundhog.Postgresql
import qualified Database.PostgreSQL.Simple.Types as PG
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw, executeQ)

withSavepoint
  :: (PostgresRaw m, MonadBaseControl IO m, Exception e)
  => String -> m a -> m (Either e a)
withSavepoint name action = do
  let savePt = PG.Identifier $ T.pack name
  [executeQ|SAVEPOINT ?savePt|]
  result <- catch (Right <$> action) $ \e -> return (Left e)
  case result of
    Left _ -> do
      [executeQ|ROLLBACK TO SAVEPOINT ?savePt|]
    Right _ -> do
      [executeQ|RELEASE SAVEPOINT ?savePt|]
  return result

--TODO: Ensure Tasks are always properly indexed
--TODO: Use Notifications to start the worker promptly
--TODO: Use a single `update returning` to take things, or consider using postgres locking
--TODO: Alert when jobs are failing
--TODO: Recover from failed jobs somehow
--TODO: Run more than one worker at a time sometimes
--TODO: Timeout workers

taskWorker
  :: forall m v c input b key a pk ready
  .  ( MonadLogger m
     , MonadIO m
     , MonadBaseNoPureAborts IO m
     , Projection input a
     , ProjectionDb input Postgresql
     , ProjectionRestriction input (RestrictionHolder v c)
     , EntityConstr v c
     , PrimitivePersistField b
     , SinglePersistField b
     , NeverNull b
     , Unifiable (SubField Postgresql v c (Maybe b)) (Maybe b)
     , ProjectionDb key Postgresql
     , ProjectionRestriction key (RestrictionHolder v c)
     , Projection key pk
     , Unifiable key pk
     , Expression Postgresql (RestrictionHolder v c) pk
     , Expression Postgresql (RestrictionHolder v c) key
     , ready ~ Cond Postgresql (RestrictionHolder v c)
     )
  => input
  -> key -- ^ MUST project a unique field of the record; otherwise, results may be stored in the wrong record
  -> ready
  -> Field v c (Task b)
  -> (a -> DbPersist Postgresql m (m (DbPersist Postgresql m b))) -- ^ Given the projected value, run some READ ONLY sql, then do an action, then run some READ WRITE sql and return a value to fill in the Task
  -> Pool Postgresql
  -> Text
  -> m Bool
taskWorker input pk ready f go db workerName = rawTaskWorker
  input
  pk
  (isFieldNothing (f ~> Task_resultSelector) &&. ready)
  (f ~> Task_rawSelector)
  (flip (fmap . fmap . fmap) go $ \go' taskId -> do
     (res :: b) <- go'
     update
       (pure @[] $ f ~> Task_resultSelector =. Just res)
       (pk ==. taskId)
  )
  db
  workerName

-- | WARNING: 'k' MUST project a unique field of the record; otherwise, results may be stored in the wrong record
rawTaskWorker
  :: forall m v c input key a pk ready
  .  ( MonadLogger m
     , MonadIO m
     , MonadBaseNoPureAborts IO m
     , Projection input a
     , ProjectionDb input Postgresql
     , ProjectionRestriction input (RestrictionHolder v c)
     , EntityConstr v c
     , ProjectionDb key Postgresql
     , ProjectionRestriction key (RestrictionHolder v c)
     , Projection key pk
     , Unifiable key pk
     , Expression Postgresql (RestrictionHolder v c) pk
     , Expression Postgresql (RestrictionHolder v c) key
     , ready ~ Cond Postgresql (RestrictionHolder v c)
     )
  => input
  -> key -- ^ MUST project a unique field of the record; otherwise, results may be stored in the wrong record
  -> ready
  -> SubField Postgresql v c RawTask
  -> (a -> DbPersist Postgresql m (m (pk -> DbPersist Postgresql m ()))) -- ^ Given the projected value, run some READ ONLY sql, then do an action, then run some READ WRITE sql and return a value to fill in the Task
  -> Pool Postgresql
  -> Text
  -> m Bool
rawTaskWorker input pk ready f go db workerName = do
  checkedOutValue <- runDb (Identity db) $ do
    qe <- project1 (pk, input) $ isFieldNothing (f ~> RawTask_checkedOutBySelector) &&. ready
    forM qe $ \(taskId, a) -> do
      now <- getTime
      update
        [ f ~> RawTask_checkedOutBySelector =. Just workerName
        , f ~> RawTask_checkedOutAtSelector =. Just now]
        $ pk ==. taskId
      result <- withSavepoint "Rhyolite.Backend.TaskWorker[1]" $ (,) taskId <$> go a
      case result of
        Right _ -> pure ()
        Left (e :: SomeException) -> update
          [ f ~> RawTask_failedSelector =. (Just . T.pack $ "Step 1:" <> show e)
          ]
          $ pk ==. taskId
      return result

  case checkedOutValue of
    Nothing -> pure False
    Just (Left bad) -> throw bad
    Just (Right (taskId, action)) -> do
      followupOrError <- catch (Right <$> action) $ return . Left
      finallError <- case followupOrError of
        Left (e :: SomeException) -> Rhyolite.Backend.DB.runDb (Identity db) $ do
          update
            [ f ~> RawTask_failedSelector =. (Just . T.pack $ "Step 2:" <> show e)
            ]
            (pk ==. taskId)
          return $ Just e
        Right followup -> Rhyolite.Backend.DB.runDb (Identity db) $ do
          bOrError <- withSavepoint "Rhyolite.Backend.TaskWorker[1]" $ followup taskId
          case bOrError of
            Left (e :: SomeException) -> do
              update
                [ f ~> RawTask_failedSelector =. (Just . T.pack $ "Step 3:" <> show e)
                ]
                (pk ==. taskId)
              return $ Just e
            Right () -> do
              update
                [ f ~> RawTask_checkedOutBySelector =. (Nothing :: Maybe Text)
                , f ~> RawTask_checkedOutAtSelector =. (Nothing :: Maybe UTCTime)
                ]
                (pk ==. taskId)
              return Nothing
      case finallError of
        Just e -> throw e
        Nothing -> pure True

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
      wakeupRest <- if (n > 1)
        then go (n-1)
        else return $ return ()
      return (wakeup *> wakeupRest)
