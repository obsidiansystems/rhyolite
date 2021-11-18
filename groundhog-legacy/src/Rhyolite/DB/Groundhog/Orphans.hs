{-# Language FlexibleContexts #-}
{-# Language UndecidableInstances #-}
{-# options_ghc -fno-warn-orphans #-}
module Rhyolite.DB.Groundhog.Orphans where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.Id.Class

instance (FromField (IdData a)) => FromField (Id a) where
  fromField f mbs = fmap Id (fromField f mbs)

instance (ToField (IdData a)) => ToField (Id a) where
  toField (Id x) = toField x
