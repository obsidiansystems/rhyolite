{-# LANGUAGE DeriveGeneric #-}
module Rhyolite.Schema.Task where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

-- | A more primitive notion of task where no value is produced, and nothing
-- tracks whether the job is done. [This is comparable to a "raw mutex" / "raw
-- mvar" that doesn't guard any data by construction.]
--
-- It is up to the user of this to ensure that a successful task run will
-- invalidate the "ready" conditions so the job isn't run again and again
-- ad-infinitum.
--
-- This is useful for jobs where it's inconvenient to track "job complete"
-- separate from the other job ready conditions.
data RawTask = RawTask
  { _rawTask_checkedOutBy :: !(Maybe Text) -- The backend node that has checked out this rawTask --TODO: Use session IDs instead of Text
  , _rawTask_checkedOutAt :: !(Maybe UTCTime)
  , _rawTask_failed :: !(Maybe Text)
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON RawTask
instance FromJSON RawTask

rawEmpty :: RawTask
rawEmpty = RawTask
  { _rawTask_checkedOutBy = Nothing
  , _rawTask_checkedOutAt = Nothing
  , _rawTask_failed = Nothing
  }

-- | A value in the database whose presence indicates that some external work
-- should be performed.  The work will produce a value of type 'a'
data Task a = Task
  { _task_result :: !(Maybe a)
  , _task_raw :: RawTask
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON a => ToJSON (Task a)
instance FromJSON a => FromJSON (Task a)

empty :: Task a
empty = Task
  { _task_result = Nothing
  , _task_raw = rawEmpty
  }
