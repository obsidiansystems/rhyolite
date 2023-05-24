{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Beam.View.Table where

import Data.Aeson
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Database.Beam.Schema.Tables

deriving instance (ToJSONKey (PrimaryKey tbl Identity), ToJSON (tbl Identity)) => ToJSON (TableView tbl)
deriving instance (FromJSONKey (PrimaryKey tbl Identity), FromJSON (tbl Identity), Ord (PrimaryKey tbl Identity)) => FromJSON (TableView tbl)

newtype TableView tbl = TableView { unTableView :: Map (PrimaryKey tbl Identity) (tbl Identity) } --Invariant: the primary key of each value is equal to the corresponding key

deriving instance (Eq (PrimaryKey tbl Identity), Eq (tbl Identity)) => Eq (TableView tbl)

deriving instance (Show (PrimaryKey tbl Identity), Show (tbl Identity)) => Show (TableView tbl)

sortTableView :: (Ord (PrimaryKey tbl Identity), Foldable f, Table tbl) => f (tbl Identity) -> TableView tbl
sortTableView = TableView . foldMap (\row -> Map.singleton (primaryKey row) row)
