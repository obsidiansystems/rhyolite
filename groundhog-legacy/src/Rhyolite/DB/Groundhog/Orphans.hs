{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language UndecidableInstances #-}
{-# options_ghc -fno-warn-orphans #-}
module Rhyolite.DB.Groundhog.Orphans where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Database.Groundhog.Postgresql.Array
import Database.Id.Class
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

instance (FromField (IdData a)) => FromField (Id a) where
  fromField f mbs = fmap Id (fromField f mbs)

instance (ToField (IdData a)) => ToField (Id a) where
  toField (Id x) = toField x

deriving instance Ord (Array BS.ByteString)
deriving instance Ord (Array LBS.ByteString)
deriving instance Ord (Array Text)

deriving instance Read (Array BS.ByteString)
deriving instance Read (Array LBS.ByteString)
deriving instance Read (Array Text)
