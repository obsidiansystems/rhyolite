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

-- | The 'Task' type describes how to use a database table to check out tasks
-- and report the results back.
data Task be table payload checkout result = Task
  { _task_filter :: forall s. table (QExpr be s) -> QExpr be s Bool
  -- ^ How to filter unclaimed task rows
  , _task_payload :: forall s. table (QExpr be s) -> QExpr be s payload
  -- ^ How to extract the payload from the row
  , _task_checkedOutBy :: forall x. Lens' (table x) (C x (Maybe checkout))
  -- ^ How the field which records a checkout is embedded within a row;
  -- this allows both reading and writing.
  , _task_hasRun :: forall x. Lens' (table x) (C x Bool)
  -- ^ Which field indicates that the task result has been checked in.
  , _task_result :: forall x. Lens' (table x) (result x)
  -- ^ How the result data is embedded within a row.
  -- Note that it has to be a Beamable type in its own right, so if the
  -- result is a mere Columnar type it should be wrapped in a newtype.
  }
