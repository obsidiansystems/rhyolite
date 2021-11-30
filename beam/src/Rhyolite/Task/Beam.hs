{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-|
Description : Contains definitions for tasks, and tables that have tasks.

For more details on how a Task is defined, refer to the Task data type below.

This module should be used when there is a list of tasks to be processed, and a limited number of workers available
to process those tasks. The workers will pick off tasks one by one, and store the results back in the table of tasks.
-}
module Rhyolite.Task.Beam where

import Control.Lens.TH
import Data.Text (Text)

import Database.Beam

-- | Data type for tables that have tasks embedded in them.
-- Specifically, this means tables that have the following 3 fields (columns)
-- 1. A task payload, which will be used an an input to the task worker.
-- 2. A result field, containg an optional value.
-- 3. A checked out by field, containing the name of the worker that checked out this task.
-- For unclaimed tasks, both fields 2 and 3 are null (Nothing).
-- When a task is checked out by a worker, the checkout out field is set to the worker's name. Result is still null.
-- When a task is completed, the result field is filled with the result, and the checked out field is set to null.
-- The type of result is dependent upon the task, hence the functional dependency between the table and the result type.
data Task i o f = Task
  { _taskPayload :: C f i -- ^ Used as an input to the worker to which this task will be assigned.
  , _taskResult :: C f (Maybe o) -- ^ Will contain the output of the task worker, once it is available. Till then, it will be Nothing.
  , _taskCheckedOutBy :: C f (Maybe Text) -- ^ Will contain the task worker name. It is equal to Nothing till it gets assigned.
  } deriving (Generic, Beamable)

deriving instance (Eq i, Eq o) => Eq (Task i o Identity)

makeLenses ''Task
