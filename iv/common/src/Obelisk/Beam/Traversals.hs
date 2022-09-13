-- | Various traversals of Beam-style records
module Obelisk.Beam.Traversals
  ( mapTablesWithArgDict
  , traverseTablesWithArgDict
  , zipTablesWithLeftArgDict
  , zipTablesWithLeftArgDict'
  , sequenceMaybeBeamable
  ) where

import Database.Beam.Schema.Tables
import Obelisk.Beam.Constraints
import Obelisk.Beam.DZippable
import Data.Functor.Identity

newtype PAP m g h a = PAP (g a -> m (h a))

mapTablesWithArgDict :: forall c db f g. (HasT c db, DZippable db) => (forall a. c a => f a -> g a) -> db f -> db g
mapTablesWithArgDict f a = runIdentity $ traverseTablesWithArgDict @c  (pure . f) a

traverseTablesWithArgDict :: forall c db f g m. (Applicative m, HasT c db, DZippable db) => (forall a. c a => f a -> m (g a)) -> db f -> m (db g)
traverseTablesWithArgDict f a = zipTablesWithLeftArgDict @c  (\_ -> f) a a

zipTablesWithLeftArgDict :: forall c db f g h m. (Applicative m, HasT c db, DZippable db) => (forall a. c a => f a -> g a -> m (h a)) -> db f -> db g -> m (db h)
zipTablesWithLeftArgDict f a b = dzip (\(PAP f') g -> f' g) (hoistWithArgDictT @db @c (PAP . f) a :: db (PAP m g h)) b

zipTablesWithLeftArgDict' :: forall c db f g h m. (Applicative m, HasT' c db f, DZippable db) => (forall a. c (f a) => f a -> g a -> m (h a)) -> db f -> db g -> m (db h)
zipTablesWithLeftArgDict' f a b = dzip (\(PAP f') g -> f' g) (hoistWithArgDictT' @db @c @f (PAP . f) a :: db (PAP m g h)) b

--TODO: Make DZippable (or something like it) a superclass of Beamable, and then implement this in terms of that
sequenceMaybeBeamable :: (Beamable table, Applicative f, f ~ Maybe) => table f -> f (table Identity)
sequenceMaybeBeamable t = zipBeamFieldsM (\(Columnar' a) _ -> Columnar' <$> a) t t
