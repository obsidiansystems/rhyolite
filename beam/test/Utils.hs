{-# Language FlexibleContexts #-}
module Utils where

import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Control.Monad (void, when)
import Data.Int (Int32)
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

type Work = Int32 -> Serializable (IO (Serializable Bool))

insertTestTasks :: Connection -> [TestTask] -> IO ()
insertTestTasks c = runBeamPostgres c . runInsert . insert (_tasks tasksDb) . insertValues

createTaskWorker :: Connection -> Work -> Text -> IO Bool
createTaskWorker c = taskWorker c (_tasks tasksDb) (val_ True) taskDetails

allTestTasks :: Connection -> IO [TestTask]
allTestTasks c =
  runBeamPostgres c $ runSelectReturningList $
    select $ all_ (_tasks tasksDb)

justOneTestTask :: Connection -> IO (Maybe TestTask)
justOneTestTask c =
  runBeamPostgres c $ runSelectReturningOne $
    select $ all_ (_tasks tasksDb)

spawnTaskWorker :: Work -> Connection -> Text -> IO ()
spawnTaskWorker work conn = void . forkIO . void . createTaskWorker conn work

toggleBoolIORef :: IORef Bool -> Bool -> Work
toggleBoolIORef boolRef flag = const $ pure $ do
  when flag $ throw TestException
  atomicModifyIORef boolRef (\b -> (not b, ()))
  pure $ pure True
