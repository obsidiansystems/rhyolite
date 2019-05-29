{-# LANGUAGE DeriveGeneric #-}
module Rhyolite.Schema.Task where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

-- | A value in the database whose presence indicates that some external work
-- should be performed.  The work will produce a value of type 'a'
data Task a = Task
  { _task_result :: !(Maybe a)
  , _task_checkedOutBy :: !(Maybe Text) -- The backend node that has checked out this task --TODO: Use session IDs instead of Text
  , _task_checkedOutAt :: !(Maybe UTCTime)
  , _task_failed :: !(Maybe Text)
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON a => ToJSON (Task a)
instance FromJSON a => FromJSON (Task a)

empty :: Task a
empty = Task
  { _task_result = Nothing
  , _task_checkedOutBy = Nothing
  , _task_checkedOutAt = Nothing
  , _task_failed = Nothing
  }
