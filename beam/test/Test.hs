{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens.TH
import Control.Monad (void)
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Int (Int32)
import Data.List (sort)
import Data.Pool (withResource)
import Data.String (fromString)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Gargoyle.PostgreSQL.Connect

import Test.Hspec

import Database.PostgreSQL.Serializable
import Rhyolite.Task.Beam
import Rhyolite.Task.Beam.Worker

data TestTaskT f = TestTask
  { _taskDetails :: Task Text Bool f
  , _taskId :: C f Int32
  } deriving (Generic, Beamable)

makeLenses ''TestTaskT

type TestTask = TestTaskT Identity
type TestTaskId = PrimaryKey TestTaskT Identity

deriving instance Eq TestTask

instance Table TestTaskT where
   data PrimaryKey TestTaskT f = TestTaskId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = TestTaskId . _taskId

newtype TestTasksDb f = TestTasksDb
  { _tasks :: f (TableEntity TestTaskT) }
    deriving (Generic, Database be)

tasksDb :: DatabaseSettings be TestTasksDb
tasksDb = defaultDbSettings `withDbModification`
  dbModification {
    _tasks =
      modifyTableFields (TestTask taskFields "id")
  }
  where
    taskFields = Task (fromString "payload") (fromString "result") (fromString "checked_out_by")

setupDB :: IO Connection
setupDB = do
  dbConn <- connect defaultConnectInfo
  execute dbConn "create table tasks (payload varchar(30) NOT NULL, result boolean, checked_out_by varchar(30), id integer PRIMARY KEY);" ()
  pure dbConn

closeDB :: Connection -> IO ()
closeDB dbConn = do
  execute dbConn "drop table tasks;" ()
  close dbConn

-- withDB :: (Connection -> IO ()) -> IO ()
-- withDB = bracket setupDB closeDB

withDB :: (Connection -> IO ()) -> IO ()
withDB f = withDb "db" $ \pool -> withResource pool f

instance MonadLogger IO where
  monadLoggerLog _ _ _ msg = print $ toLogStr msg

instance MonadLoggerIO IO where
  askLoggerIO = pure (\_ _ _ logStr -> print logStr)

data TestException = TestException
   deriving (Show, Typeable)

instance Exception TestException

main :: IO ()
main = hspec $ around withDB $ do
  describe "taskWorker" $ do
    it "works functionally, bare minimum test" $ \c -> do
      boolRef <- newIORef False

      runBeamPostgres c $ runInsert $
        insert (_tasks tasksDb) $
        insertValues
          [ TestTask (Task "Toggle an IORef Bool" Nothing Nothing) 1 ]

      -- The worker should not have failed
      taskWorker c (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          atomicModifyIORef boolRef (const (True, ()))
          pure $ pure True
        ) "Test-Worker" `shouldReturn` True

      maybeTask <- runBeamPostgres c $ runSelectReturningOne $
        select $ all_ (_tasks tasksDb)

      -- The worker should have updated the database with its task result
      case maybeTask of
        Nothing -> do
          fail "There should have been 1 row in the tasks table"
          pure ()
        Just (TestTask (Task _ result checkedOutBy) _) -> do
          result `shouldBe` Just True
          checkedOutBy `shouldBe` Nothing

      -- The worker should have completed its task
      readIORef boolRef `shouldReturn` True
    
    it "fails, keeps row in database" $ \c -> do
      boolRef <- newIORef False

      runBeamPostgres c $ runInsert $
        insert (_tasks tasksDb) $
        insertValues
          [ TestTask (Task "Toggle an IORef Bool" Nothing Nothing) 1 ]

      -- The worker should not have failed
      taskWorker c (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          -- Throw an exception to signal failure
          throw TestException
          atomicModifyIORef boolRef (const (True, ()))
          pure $ pure True
        ) "Test-Worker" `shouldThrow` anyException

      maybeTask <- runBeamPostgres c $ runSelectReturningOne $
        select $ all_ (_tasks tasksDb)

      -- The worker should have updated the database with its task result
      case maybeTask of
        Nothing -> do
          fail "There should have been 1 row in the tasks table"
          pure ()
        Just (TestTask (Task _ _ checkedOutBy) _) -> checkedOutBy `shouldBe` Just "Test-Worker"

      -- The worker failed its task, so the ioref should still be false
      readIORef boolRef `shouldReturn` False

    it "works concurrently, 2 workers" $ \c1 -> do
      -- MVar which will be used to block both the workers once they have retrieved tasks from the table.
      awakenMVar <- newEmptyMVar :: IO (MVar ())

      -- Will be used inside worker task as an increment action
      countRef <- newIORef 0

      -- Add two separate tasks to the table
      runBeamPostgres c1 $ runInsert $
        insert (_tasks tasksDb) $
        insertValues
          [ TestTask (Task "Toggle an IORef Bool" Nothing Nothing) 1
          , TestTask (Task "Toggle an IORef Bool" Nothing Nothing) 2
          ]

      -- Spawn two workers in separate threads
      void $ forkIO $ void $ taskWorker c1 (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          -- Block on awakenMVar
          () <- readMVar awakenMVar
          atomicModifyIORef countRef (\i -> (i+1, ()))
          pure $ pure True
        ) "Test-Worker 1"

      c2 <- connect defaultConnectInfo

      void $ forkIO $ void $ taskWorker c2 (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          -- Block on awakenMVar
          () <- readMVar awakenMVar
          atomicModifyIORef countRef (\i -> (i+1, ()))
          pure $ pure True
        ) "Test-Worker 2"

      -- Wait for 500ms
      threadDelay $ 500 * 1000

      -- Both workers should have retrieved tasks from the table and blocked by now
      tasks <- runBeamPostgres c1 $ runSelectReturningList $
        select $ all_ (_tasks tasksDb)

      -- Both tasks should have been checkedout
      sort (map (_taskCheckedOutBy . _taskDetails) tasks) `shouldBe` [Just "Test-Worker 1", Just "Test-Worker 2"]

      -- Unblock both workers
      putMVar awakenMVar ()

      -- Wait for 500ms
      threadDelay $ 500 * 1000

      -- Both tasks should have been completed by now, and updated in the table
      tasks <- runBeamPostgres c1 $ runSelectReturningList $
        select $ all_ (_tasks tasksDb)

      -- Both tasks should have checked out by set to null
      map (_taskCheckedOutBy . _taskDetails) tasks `shouldBe` replicate 2 Nothing
      -- Both tasks' result should be Just True
      map (_taskResult . _taskDetails) tasks `shouldBe` replicate 2 (Just True)

      -- The increment task should also have been completed
      readIORef countRef `shouldReturn` 2

    it "never picks up a checked out or completed task" $ \c -> do
      -- Will be used later in the worker's task
      boolRef <- newIORef False

      let
        tasks =
          [ TestTask (Task "Toggle an IORef Bool" (Just True) Nothing) 1 -- Completed Task
          , TestTask (Task "Toggle an IORef Bool" Nothing (Just "Test-Worker 1")) 2 -- Task already checked out
          ]

      -- Add two separate tasks to the table
      runBeamPostgres c $ runInsert $
        insert (_tasks tasksDb) $
        insertValues tasks

      -- Spawn a worker in a separate threads
      void $ forkIO $ void $ taskWorker c (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          atomicModifyIORef boolRef (const (True, ()))
          pure $ pure True
        ) "Test-Worker 2"

      -- Wait for 3 whole seconds
      threadDelay $ 3 * 1000 * 1000

      -- Our IORef should still contain False
      readIORef boolRef `shouldReturn` False

      -- Both the tasks should not have been modified
      tasksInDb <- runBeamPostgres c $ runSelectReturningList $
        select $ all_ (_tasks tasksDb)

      tasksInDb == tasks `shouldBe` True
