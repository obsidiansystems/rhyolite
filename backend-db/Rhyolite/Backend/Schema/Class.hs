{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.Schema.Class where

import Data.Proxy (Proxy)
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql ()
import Database.Id.Class

-- This class is for data structures that aren't database entities themselves, but are built from them
-- The DerivedEntityHead of a DerivedEntity is the db entity type representing the root of the data structure.
class (PersistEntity (DerivedEntityHead v), IdData v ~ IdData (DerivedEntityHead v)) => DerivedEntity v where
  type DerivedEntityHead v :: *

class (IsUniqueKey (Key a (Unique (DefaultKeyUnique a))), IsUniqueKey (DefaultKey a)) => DefaultKeyIsUnique a where
  type DefaultKeyUnique a :: (* -> *) -> *
  defaultKeyToKey :: DefaultKey a -> Key a (Unique (DefaultKeyUnique a))

class GetByDefault a where
  getByDefault :: PersistBackend m => DefaultKey a -> m (Maybe a)

class IsSumType a ~ HFalse => HasSingleConstructor a where
  type SingleConstructor a :: (* -> *) -> *
  singleConstructor :: Proxy a -> SingleConstructor a (ConstructorMarker a)
