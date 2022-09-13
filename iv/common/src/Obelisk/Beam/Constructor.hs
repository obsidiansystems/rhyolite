-- | Tools for type-directed construction of Beam-style records
module Obelisk.Beam.Constructor
  ( emptyDatabase
  , emptyDatabaseM
  , emptyDatabaseWithArgDict
  , emptyDatabaseWithArgDictM
  ) where

import Obelisk.Beam.DZippable
import Obelisk.Beam.Constraints
import Obelisk.Beam.Traversals
import Data.Functor.Identity

--TODO: This should probably be upstreamed into 'Database' so that it doesn't need to rely on zipTables being sufficiently lazy
emptyDatabase :: DZippable db => (forall a. f a) -> db f
emptyDatabase z = runIdentity $ emptyDatabaseM $ pure z

emptyDatabaseM :: (DZippable db, Applicative m) => (forall a. m (f a)) -> m (db f)
emptyDatabaseM z = dzip (\_ _ -> z) (undefined :: db f) (undefined :: db f)

emptyDatabaseWithArgDict :: forall c db (f :: * -> *). (HasT c db, DZippable db) => (forall a. c a => f a) -> db f
emptyDatabaseWithArgDict z = runIdentity $ emptyDatabaseWithArgDictM @c $ pure z

emptyDatabaseWithArgDictM :: forall c db (f :: * -> *) m. (HasT c db, DZippable db, Applicative m) => (forall a. c a => m (f a)) -> m (db f)
emptyDatabaseWithArgDictM z = traverseTablesWithArgDict @c (\_ -> z) (emptyDatabase undefined)
