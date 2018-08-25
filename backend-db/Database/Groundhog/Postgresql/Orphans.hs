{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Database.Groundhog.Postgresql.Orphans where

import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Database.Groundhog.Postgresql.Array

deriving instance Ord (Array BS.ByteString)
deriving instance Ord (Array LBS.ByteString)
deriving instance Ord (Array Text)

deriving instance Read (Array BS.ByteString)
deriving instance Read (Array LBS.ByteString)
deriving instance Read (Array Text)
