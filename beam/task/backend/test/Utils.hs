{-# Language FlexibleContexts #-}
module Utils where

import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Control.Monad (void, when)
import Data.Int (Int64)
import Data.IORef
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Serializable
import Rhyolite.Task.Beam.Worker
import Types

type Work = Int64 -> Pg (IO (Pg (WrappedColumnar (Maybe Bool) Identity)))

insertTestTasks :: Connection -> [TestTask] -> IO ()
insertTestTasks c = runBeamPostgres c . runInsert . insert (_testTasksDb_tasks tasksDb) . insertValues

createTaskWorker :: Connection -> Work -> Text -> IO Bool
createTaskWorker conn work wId = taskWorker
  conn
  (_testTasksDb_tasks tasksDb)
  testTask
  (\_ -> work . unWrappedColumnar)
  wId

allTestTasks :: Connection -> IO [TestTask]
allTestTasks c =
  runBeamPostgres c $ runSelectReturningList $
    select $ all_ (_testTasksDb_tasks tasksDb)

justOneTestTask :: Connection -> IO (Maybe TestTask)
justOneTestTask c =
  runBeamPostgres c $ runSelectReturningOne $
    select $ all_ (_testTasksDb_tasks tasksDb)

spawnTaskWorker :: Work -> Connection -> Text -> IO ()
spawnTaskWorker work conn = void . forkIO . void . createTaskWorker conn work

toggleBoolIORef :: IORef Bool -> Bool -> Work
toggleBoolIORef boolRef flag = const $ pure $ do
  when flag $ throw TestException
  atomicModifyIORef boolRef (\b -> (not b, ()))
  pure $ pure $ WrappedColumnar (Just True)
