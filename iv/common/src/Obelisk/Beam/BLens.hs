-- | Lenses for Beam-style records (i.e. records where each field is wrapped in a functor)
-- Importantly, the lenses are agnostic to the actual functor used
module Obelisk.Beam.BLens
  ( BLens (..)
  , HasBLenses (..)
  ) where

import Control.Lens (Lens')

newtype BLens db e = BLens (forall f. Lens' (db f) (f e))

--TODO: Can this be replaced with HasFields?
class HasBLenses db where
  -- | Get all the BLenses for a given Beam-style record type
  -- BLens is itself a functor that can be used with the Beam-style record.  This allows us to retrieve all the BLenses for a given datastructure as an instance of that datastructure.
  blenses :: db (BLens db)
