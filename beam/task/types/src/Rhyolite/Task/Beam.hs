{-# Language DeriveAnyClass #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-|
Description : Models the interface to a work queue that is stored in a database table.

The 'Task' type in this module describes how to interact with a 'Table' that tracks tasks that can be checked out and processed by workers in a distributed but serializable fashion. In this model tasks are not locked by workers but are instead atomically checked out for processing and then later atomically checked in when processing is done.
-}
module Rhyolite.Task.Beam where

import Database.Beam

-- | The Task type describes how to use a database table to check out tasks
-- and record back the results after processing them.
--
-- One can use 'Rhyolite.Task.Beam.Worker.taskWorker' to process a job
-- given a 'Task' representing the job queue's interface.
data Task be table payload checkout result = Task
  { _task_filter :: forall s. table (QExpr be s) -> QExpr be s Bool
  -- ^ A user-supplied predicate for choosing suitable tasks. This
  -- is combined with the logic that handles checkout and checkin
  -- in 'Rhyolite.Task.Beam.Worker.taskWorker'.
  , _task_payload :: forall s. table (QExpr be s) -> payload (QExpr be s)
  -- ^ How to extract the payload from the row
  , _task_checkedOutBy :: forall x. table x -> C x (Maybe checkout)
  -- ^ How the field which records a checkout is embedded within a row;
  -- a lens allows both reading and writing.
  , _task_result :: forall s. table (QField s) -> result (QField s)
  -- ^ How the result data is embedded within a row.
  -- Note that it has to be a 'Beamable' type in its own right, so if the
  -- result is a mere 'Columnar' type it should be wrapped in a newtype.
  }
