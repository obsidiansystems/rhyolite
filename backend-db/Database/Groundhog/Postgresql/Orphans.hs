-- | Orphan instances for 'Ord' and 'Read' for arrays of 'ByteString' and 'Text'

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Groundhog.Postgresql.Orphans where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Database.Groundhog.Postgresql.Array

deriving instance Ord (Array BS.ByteString)
deriving instance Ord (Array LBS.ByteString)
deriving instance Ord (Array Text)

deriving instance Read (Array BS.ByteString)
deriving instance Read (Array LBS.ByteString)
deriving instance Read (Array Text)
