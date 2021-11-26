{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
-- | Contains definitions for tasks, and tables that have tasks.
module Rhyolite.Task.Beam where

import Control.Lens.TH
import Control.Lens.Type
import Data.Text (Text)

import Database.Beam

-- TODO Update the documentation here
-- | Data Family for tables that have tasks embedded in them.
-- Specifically, this means tables that have the following 2 fields (columns)
-- 1. A result field, containg an optional value
-- 2. A checked out by field, containing the name of the worker that checked out this task
-- For unclaimed tasks, both these fields are null (Nothing).
-- When a task is checked out by a worker, the checkout out field is set to the worker's name. Result is still null.
-- When a task is completed, the result field is filled with the result, and the checked out field is set to null.
-- The type of result is dependent upon the task, hence the functional dependency between the table and the result type.
data Task i o f = Task
  { _taskPayload :: C f i
  , _taskResult :: C f (Maybe o)
  , _taskCheckedOutBy :: C f (Maybe Text)
  } deriving (Generic, Beamable)

makeLensesWith (lensRulesFor
  [ ("_taskPayload", "taskPayload'")
  , ("_taskResult", "taskResult'")
  , ("_taskCheckedOutBy", "taskCheckoutBy'")
  ]) ''Task

class HasTask t i o | t -> i o where
  task :: forall f. Lens' (t f) (Task i o f)
  taskCheckedOutBy :: forall f. Lens' (t f) (C f (Maybe Text))
  taskCheckedOutBy = task . taskCheckoutBy'
  taskPayload :: forall f. Lens' (t f) (C f i)
  taskPayload = task . taskPayload'
  taskResult :: forall f. Lens' (t f) (C f (Maybe o))
  taskResult = task . taskResult'

instance HasTask (Task i o) i o where
  task = id
