{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forM_, void, zipWithM_)
import Data.Int (Int32)
import Data.IORef
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Pool (Pool, withResource)
import Data.Text (Text, pack)
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Serializable
import Gargoyle.PostgreSQL.Connect
import System.FilePath
import System.Posix.Temp
import Test.Hspec

import Rhyolite.Task.Beam
import Rhyolite.Task.Beam.Worker

import Types
import Utils

-- TODO : Use beam-automigrate
-- TODO : Check how we can achieve complete haddock coverage even with lens

setupTable :: Pool Connection -> IO (Pool Connection)
setupTable pool = do
  withResource pool $ \dbConn ->
    execute dbConn "create table tasks (payload integer NOT NULL, result boolean, checked_out_by varchar(30), id integer PRIMARY KEY);" ()
  pure pool

deleteTable :: Pool Connection -> IO ()
deleteTable pool = void $ withResource pool $ \dbConn -> execute dbConn "drop table tasks;" ()

withTables :: Pool Connection -> (Connection -> IO ()) -> IO ()
withTables pool = bracket (setupTable pool) deleteTable . const . withResource pool

-- Time taken to process one task - 100ms
timeForOneTask :: Int
timeForOneTask = 100 * 1000

main :: IO ()
main = do
  tmp <- mkdtemp "psql-test"
  let dbPath = tmp </> "db"

  withDb dbPath $ \pool ->
    hspec $ around (withTables pool) $ do
      describe "taskWorker" $ do
        -- TEST 1
        --   Creates a task that toggles a boolean. Tests functionally that everything works.
        it "works functionally, bare minimum test" $ \c -> do
          -- SETUP
          -- Create an IORef which will be toggled by the worker
          -- Add a task to the table
          let
            initBool = False
          boolRef <- newIORef initBool
          insertTestTasks c [ createTask 1 ]

          -- ACT
          -- Create a task worker which toggles the boolean IORef
          ret <- createTaskWorker c (toggleBoolIORef boolRef False) "Test-Worker"

          -- ASSERT
          -- The worker should have updated the database with its task result
          -- The worker should not have failed
          -- The worker should have completed its task
          maybeTask <- justOneTestTask c
          case maybeTask of
            Nothing -> do
              fail "There should have been 1 row in the tasks table"
              pure ()
            Just (TestTask (Task _ result checkedOutBy) _) -> do
              result `shouldBe` Just True
              checkedOutBy `shouldBe` Nothing
          ret `shouldBe` True
          readIORef boolRef `shouldReturn` not initBool

        -- TEST 2
        --   Creates a task that fails. Tests that even on failure, the task is checked out by the worker.
        it "fails, keeps row in database" $ \c -> do
          -- SETUP
          -- Create an IORef bool that should be toggled by the worker task.
          -- Insert a single task into the table
          let
            initBool = False
            workerName = "Test-Worker"
          boolRef <- newIORef initBool
          insertTestTasks c [ createTask 1 ]

          -- ACT
          -- Create a task worker which fails (throws an exception)
          e <- try $ createTaskWorker c (toggleBoolIORef boolRef True) workerName

          -- ASSERT
          -- The task should have failed with a `TestException`
          -- The worker should have updated the database with its task result
          -- The worker failed its task, so the ioref should still be false
          e `shouldBe` Left TestException
          maybeTask <- justOneTestTask c
          case maybeTask of
            Nothing -> do
              fail "There should have been 1 row in the tasks table"
              pure ()
            Just (TestTask (Task _ _ checkedOutBy) _) -> checkedOutBy `shouldBe` Just workerName
          readIORef boolRef `shouldReturn` initBool

        -- TEST 3
        --   Adds two tasks to the table, then creates two workers. Both these workers will be blocked from
        --   finishing their task by reading an MVar. By this time they should have checked out one task each.
        --   Once this has been verified, we unblock both tasks, and let them finish.
        --   NOTE: We will require two connections for this test,
        --         since Postgresql shows a warning when we run 2 transactions from the same connection
        it "works concurrently, 2 workers" $ \c -> do
          -- SETUP
          -- Create an MVar which will be used to block both the workers once they have retrieved tasks from the table.
          -- Create an IORef Int which will be used inside worker task as an increment action
          -- Add two separate tasks to the table
          -- Create the second connection
          let
            initCount = 0
            workerNames = ["Test-Worker 1", "Test-Worker 2"]
          awakenMVar <- newEmptyMVar :: IO (MVar ())
          countRef <- newIORef initCount
          insertTestTasks c $ map createTask [1, 2]
          withResource pool $ \c2 -> do

            -- ACT
            -- Spawn two workers in separate threads
            let
              work = const $ pure $ do
                -- Block on awakenMVar
                () <- readMVar awakenMVar
                atomicModifyIORef countRef (\i -> (i+1, ()))
                pure $ pure True
            zipWithM_ (spawnTaskWorker work) [c, c2] workerNames

            -- ASSERT
            -- Wait for both workers to block
            -- Both workers should have retrieved tasks from the table and blocked by now
            -- Both tasks should have been checkedout
            threadDelay $ 3 * timeForOneTask
            tasks <- allTestTasks c
            sort (map (_taskCheckedOutBy . _taskDetails) tasks) `shouldBe` map Just workerNames

            -- ACT
            -- Unblock both workers
            -- Wait for both workers to finish
            putMVar awakenMVar ()
            threadDelay $ 3 * timeForOneTask

            -- ASSERT
            -- Both tasks should have been completed by now, and updated in the table
            -- Both tasks should have `checked_out_by` set to null
            -- Both tasks' result should be Just True
            -- The increment task should also have been completed
            tasks <- allTestTasks c
            map (_taskCheckedOutBy . _taskDetails) tasks `shouldBe` replicate 2 Nothing
            map (_taskResult . _taskDetails) tasks `shouldBe` replicate 2 (Just True)
            readIORef countRef `shouldReturn` (initCount + 2)

        -- TEST 4
        --   Create a worker with a task that will never run, since the table only has
        --   completed or already checked-out tasks.
        it "never picks up a checked out or completed task" $ \c -> do
          -- SETUP
          -- Create a boolean IORef that will be modified by the worker
          -- Add two separate tasks to the table
          let
            initBool = False
            tasks =
              [ TestTask (Task 1 (Just True) Nothing) 1 -- Completed Task
              , TestTask (Task 2 Nothing (Just "Test-Worker 1")) 2 -- Task already checked out
              ]
          boolRef <- newIORef initBool
          insertTestTasks c tasks

          -- ACT
          -- Spawn a worker in a separate thread
          spawnTaskWorker (toggleBoolIORef boolRef False) c "Test-Worker 2"

          -- ASSERT
          -- Wait for some time
          -- Our IORef should still contain its initial value
          -- Both the tasks should not have been modified
          threadDelay $ 3 * timeForOneTask
          readIORef boolRef `shouldReturn` initBool
          tasksInDb <- allTestTasks c
          tasksInDb == tasks `shouldBe` True

        -- TEST 5
        -- Creates some workers, and a bigger number of tasks. Workers run repeatedly until they run out of tasks.
        -- All tasks should be processed, and each task should only be processed once.
        it "runs parallel taskWorkers on a number of tasks" $ \c -> do
          -- SETUP
          -- Create a limited number of tasks
          -- Create an IORef that stores a map, from Task IDs (Int32) -> Set of worker names that processed the task (Set Text)
          let
            taskCount = 1000
            threadCount = 8
            tasks = map createTask [1..fromIntegral taskCount]
          mapRef <- newIORef (M.empty :: M.Map Int32 (S.Set Text))

          -- ACT
          -- Insert all the tasks into the table
          -- Wait for some time
          -- Spawn a number of threads, each thread repeatedly runs a taskWorker,
          --   which updates the map with an entry for the task along with the worker name.
          let
            work workerName testTaskId = pure $ do
              atomicModifyIORef mapRef (\old -> (M.insertWith S.union testTaskId (S.singleton workerName) old, ()))
              pure $ pure True
            repeatWork conn threadId = do
              let workerName = "Task-Worker " <> pack (show threadId)
              b <- createTaskWorker conn (work workerName) workerName
              if b
                then repeatWork conn threadId
                else pure ()
          insertTestTasks c tasks
          threadDelay $ 3 * timeForOneTask
          forM_ [1..threadCount] $ \tid ->
            void $ forkIO $
              withResource pool $ \conn ->
                repeatWork conn tid

          -- ASSERT
          -- Wait for some time
          -- All tasks' `checked out by` should have been set to null
          -- All tasks should have the result of the work set, as Just True
          -- Each task should have been checked out at some time, with an entry in the map
          -- Each task should have been checked out only once.
          threadDelay $ (taskCount * timeForOneTask) `div` threadCount
          taskMap <- readIORef mapRef
          tasks <- allTestTasks c
          map (_taskCheckedOutBy . _taskDetails) tasks `shouldBe` replicate taskCount Nothing
          map (_taskResult . _taskDetails) tasks `shouldBe` replicate taskCount (Just True)
          M.keys taskMap `shouldBe` [1..fromIntegral taskCount]
          all (\set -> S.size set == 1) (M.elems taskMap) `shouldBe` True
