{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Backend.Schema.Class where

import Data.Proxy (Proxy)
import Data.Some (Some)
import Database.Groundhog.Core
import Database.Groundhog.Generic.Sql ()

import Rhyolite.Schema (HasId (..))

class HasId a => DefaultKeyId a where
  toIdData :: Proxy a -> DefaultKey a -> IdData a
  fromIdData :: Proxy a -> IdData a -> DefaultKey a

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

type IdDataIs a b = IdData a ~ b

class (PersistEntity e, Enum (Some (ConstructorTag e)), Bounded (Some (ConstructorTag e))) => RhyoliteEntity e where
  data ConstructorTag e :: ((* -> *) -> *) -> *
  tagToConstructor :: ConstructorTag e c -> c (ConstructorMarker e)
  tagToConstructorDef :: ConstructorTag e c -> ConstructorDef

class (Constructor c, EntityConstr e c, RhyoliteEntity e) => TaggedConstructor e c where
  constructorToTag :: c (ConstructorMarker e) -> ConstructorTag e c

class (Constructor c, EntityConstr e c, RhyoliteEntity e, Enum (Some (KeyTag e c)), Bounded (Some (KeyTag e c))) => KeyedConstructor e c where
  data KeyTag e c :: * -> *
  defaultKeyTag :: ConstructorTag e c -> Some (KeyTag e c)
  keyFields :: Some (KeyTag e c) -> [Some (Field e c)]
