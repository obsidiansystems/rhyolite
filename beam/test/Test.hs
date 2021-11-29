{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Main where

import Control.Exception (bracket)
import Control.Lens.TH
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text

import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple

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

withDB :: (Connection -> IO ()) -> IO ()
withDB = bracket setupDB closeDB

instance MonadLogger IO where
  monadLoggerLog _ _ _ msg = print $ toLogStr msg

instance MonadLoggerIO IO where
  askLoggerIO = pure (\_ _ _ logStr -> print logStr)

main :: IO ()
main = hspec $ around withDB $ do
  describe "taskWorker" $ do
    it "works functionally, bare minimum test" $ \c -> do
      ioBool <- newIORef False

      runBeamPostgres c $ runInsert $
        insert (_tasks tasksDb) $
        insertValues
          [ TestTask (Task "Toggle an IORef Bool" Nothing Nothing) 1 ]

      -- The worker should not have failed
      taskWorker c (_tasks tasksDb) (val_ True) taskDetails (\_ -> do
        pure $ do
          atomicModifyIORef ioBool (const (True, ()))
          pure $ pure True
        ) "Test-Worker" `shouldReturn` True

      maybeTask <- runBeamPostgres c $ runSelectReturningOne $
        select $ all_ (_tasks tasksDb)

      -- The worker should have updated the database with its task result
      case maybeTask of
        Nothing -> do
          fail "There should have been 1 row in the tasks table"
          pure ()
        Just (TestTask (Task _ result _) _) -> result `shouldBe` Just True

      -- The worker should have completed its task
      readIORef ioBool `shouldReturn` True
