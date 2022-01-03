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

import Control.Lens

import Database.Beam

data CommitTask
   = CommitTask_Commit
   -- ^ The task is completed and should be recorded as such
   | CommitTask_Delete
   -- ^ The task is completed and should be deleted from the table

-- | The 'Task' type describes how to use a database table to check out tasks
-- and report the results back.
data Task be table checkout = Task
  { _task_filter :: forall s. table (QExpr be s) -> QExpr be s Bool
  -- ^ How to filter unclaimed task rows
  , _task_checkedOutBy :: forall x. Lens' (table x) (C x (Maybe checkout))
  -- ^ How the field which records a checkout is embedded within a row;
  -- this allows both reading and writing.
  , _task_hasRun :: forall x. Lens' (table x) (C x Bool)
  -- ^ Which field indicates that the task result has been checked in.
  }
