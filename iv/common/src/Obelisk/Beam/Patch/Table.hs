{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Beam.Patch.Table
  ( TablePatch (..)
  , nullTablePatch
  ) where

import Data.Aeson
import Data.Functor.Identity
import Data.Monoid.DecidablyEmpty
import Database.Beam.Schema.Tables
import Obelisk.Beam.Patch.Row
import Obelisk.Beam.View.Table
import Obelisk.Reflex.Orphans ()
import Data.Patch
import Data.Patch.MapWithPatchingMove
import qualified Data.Map.Strict as Map

newtype TablePatch tbl = TablePatch { unTablePatch :: PatchMapWithPatchingMove (PrimaryKey tbl Identity) (RowPatch tbl) }

nullTablePatch :: TablePatch tbl -> Bool
nullTablePatch (TablePatch (PatchMapWithPatchingMove m)) = Map.null m

deriving instance (Show (PrimaryKey tbl Identity), Show (tbl Identity), Show (tbl Maybe)) => Show (TablePatch tbl)
deriving instance (Ord (PrimaryKey tbl Identity), Beamable tbl, Eq (tbl Maybe)) => Semigroup (TablePatch tbl)
deriving instance (Ord (PrimaryKey tbl Identity), Beamable tbl, Eq (tbl Maybe)) => Monoid (TablePatch tbl)
deriving instance (Ord (PrimaryKey tbl Identity), Beamable tbl, Eq (tbl Maybe)) => DecidablyEmpty (TablePatch tbl)
deriving instance (Ord (PrimaryKey tbl Identity), Eq (tbl Maybe), Eq (tbl Identity)) => Eq (TablePatch tbl)

deriving instance (ToJSONKey (PrimaryKey tbl Identity), ToJSON (tbl Maybe), ToJSON (tbl Identity), ToJSON (PrimaryKey tbl Identity)) => ToJSON (TablePatch tbl)
deriving instance (FromJSONKey (PrimaryKey tbl Identity), FromJSON (tbl Maybe), FromJSON (tbl Identity), FromJSON (PrimaryKey tbl Identity), Ord (PrimaryKey tbl Identity)) => FromJSON (TablePatch tbl)

instance (Beamable tbl, Ord (PrimaryKey tbl Identity)) => Patch (TablePatch tbl) where
  type PatchTarget (TablePatch tbl) = TableView tbl
  apply (TablePatch p) (TableView v) = TableView <$> apply p v
