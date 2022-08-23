{-# Language DataKinds #-}
{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# Language TypeFamilies #-}

module Types where

import Control.Exception
import Control.Lens
import Control.Monad.Logger
import Data.Int (Int64)
import Data.Proxy
import Data.String (fromString)
import Data.Text (Text)
import Database.Beam
import qualified Database.Beam.AutoMigrate as BA
import Database.Beam.Postgres

import Rhyolite.Task.Beam

data TestTaskT f = TestTaskT
  { _testTaskT_id :: Columnar f Int64
  , _testTaskT_checkedOutBy :: Columnar f (Maybe Text)
  , _testTaskT_payload :: Columnar f Int64
  , _testTaskT_result :: Columnar f (Maybe Bool)
  , _testTaskT_finished :: Columnar f Bool
  } deriving (Generic)

makeLenses ''TestTaskT

type TestTask = TestTaskT Identity
type TestTaskId = PrimaryKey TestTaskT Identity

deriving instance Eq TestTask

instance Beamable TestTaskT
instance Table TestTaskT where
  newtype PrimaryKey TestTaskT f = TestTaskId { unTestTaskId :: Columnar f Int64 }
    deriving (Generic)
  primaryKey = TestTaskId . _testTaskT_id

instance Beamable (PrimaryKey TestTaskT)

newtype WrappedColumnar a f = WrappedColumnar { unWrappedColumnar :: Columnar f a }
  deriving (Generic)

instance Beamable (WrappedColumnar a)

testTask :: Task Postgres TestTaskT (WrappedColumnar Int64) Text (WrappedColumnar (Maybe Bool))
testTask = Task
  { _task_filter = \_ -> val_ True
  , _task_payload = WrappedColumnar . _testTaskT_payload
  , _task_checkedOutBy = testTaskT_checkedOutBy
  , _task_hasRun = testTaskT_finished
  , _task_result = lens
    (\t -> WrappedColumnar (_testTaskT_result t))
    (\t (WrappedColumnar s) -> t
      { _testTaskT_result = s
      }
    )
  }

createTask :: Int64 -> TestTask
createTask i = TestTaskT
  { _testTaskT_id = i
  , _testTaskT_checkedOutBy = Nothing
  , _testTaskT_payload = i
  , _testTaskT_result = Nothing
  , _testTaskT_finished = False
  }

newtype TestTasksDb f = TestTasksDb
  { _testTasksDb_tasks :: f (TableEntity TestTaskT)
  } deriving (Generic)

instance Database be TestTasksDb

tasksDb :: DatabaseSettings be TestTasksDb
tasksDb = defaultDbSettings

tasksDbPostgres :: BA.AnnotatedDatabaseSettings Postgres TestTasksDb
tasksDbPostgres = BA.defaultAnnotatedDbSettings tasksDb

taskSchema :: BA.Schema
taskSchema = BA.fromAnnotatedDbSettings tasksDbPostgres (Proxy @'[])

data TestException = TestException
   deriving (Eq, Show, Typeable)

instance Exception TestException
