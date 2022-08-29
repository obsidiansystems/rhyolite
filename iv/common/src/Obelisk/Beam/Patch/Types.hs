{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Obelisk.Beam.Patch.Types where

import Obelisk.Beam.DZippable
import Data.Functor.Identity
import Database.Beam.Schema.Tables
import Control.Lens (Iso, iso)
import Data.Vessel (FlipAp(..))


subDbSettings :: DMappable subDb => (db (DatabaseEntity be db) -> subDb (DatabaseEntity be db)) -> DatabaseSettings be db -> DatabaseSettings be subDb
subDbSettings f db = runIdentity $ dmap (\(DatabaseEntity e) -> pure $ DatabaseEntity e) (f db)


-- TODO: Put in Data.Vessel
_FlipAp :: Iso (FlipAp a f) (FlipAp b g) (f a) (g b)
_FlipAp = iso unFlipAp FlipAp
