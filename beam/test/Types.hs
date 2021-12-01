{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module Types where

import Control.Exception
import Control.Lens
import Control.Monad.Logger
import Data.Int (Int32)
import Data.String (fromString)
import Data.Text (Text)
import Database.Beam

import Rhyolite.Task.Beam

data TestTaskT f = TestTask
  { _taskDetails :: Task Int32 Bool f
  , _taskId :: C f Int32
  } deriving (Generic, Beamable)

makeLenses ''TestTaskT

type TestTask = TestTaskT Identity
type TestTaskId = PrimaryKey TestTaskT Identity

deriving instance Eq TestTask

instance Table TestTaskT where
   data PrimaryKey TestTaskT f = TestTaskId (Columnar f Int32) deriving (Generic, Beamable)
   primaryKey = TestTaskId . _taskId

createTask :: Int32 -> TestTask
createTask i = TestTask (Task i Nothing Nothing) i

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

data TestException = TestException
   deriving (Eq, Show, Typeable)

instance Exception TestException

instance MonadLogger IO where
  monadLoggerLog _ _ _ msg = print $ toLogStr msg

instance MonadLoggerIO IO where
  askLoggerIO = pure (\_ _ _ logStr -> print logStr)
