{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Db.LiveQuery where

import Data.Constraint
import Data.Constraint.Extras
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Misc
import Data.GADT.Compare
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap (..))
import qualified Data.Map.Monoidal as MMap
import Data.Patch.MapWithPatchingMove (PatchMapWithPatchingMove(..))
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vessel (Vessel, SingleV (..), View, IdentityV (..))
import qualified Data.Vessel as Vessel
import Data.Vessel.SubVessel (SubVessel, mkSubVessel, getSubVessel, handleSubVesselSelector)
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.Beam.Schema.Tables
import Obelisk.Api
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Obelisk.View.App
import Rhyolite.SemiMap

data LiveQuery db v = LiveQuery
  { _liveQuery_view :: db (DatabaseEntity Postgres db) -> v Proxy -> ReadDb (v Identity)
  , _liveQuery_listen :: db (DatabaseEntity Postgres db) -> TablesV db (ComposeMaybe TablePatch) -> v Proxy -> ReadDbPatch (v Identity)
  }

-- | Build a LiveQuery by providing a separate query for each possible key.
-- This could be inefficient if the number of keys is large.
class QueryPerKey v where
  type QueryPerKey_Key v (innerV :: (* -> *) -> *) :: *
  type QueryPerKey_Constraint v (innerV :: (* -> *) -> *) :: Constraint
  queryPerKey :: (forall innerV. QueryPerKey_Constraint v innerV => QueryPerKey_Key v innerV -> LiveQuery db innerV) -> LiveQuery db v

instance (GCompare k, Has View k) => QueryPerKey (Vessel k) where
  type QueryPerKey_Key (Vessel k) innerV = k innerV
  type QueryPerKey_Constraint (Vessel k) innerV = View innerV
  queryPerKey f = LiveQuery
    { _liveQuery_view = \db -> Vessel.traverseWithKeyV $ \k -> _liveQuery_view (f k) db
    , _liveQuery_listen = \db p -> Vessel.traverseWithKeyV $ \k -> _liveQuery_listen (f k) db p
    }

instance (Ord k, View v) => QueryPerKey (SubVessel k v) where
  type QueryPerKey_Key (SubVessel k v) innerV = k
  type QueryPerKey_Constraint (SubVessel k v) innerV = innerV ~ v
  queryPerKey f = LiveQuery
    { _liveQuery_view = \db -> Vessel.traverseSubVessel $ \k -> _liveQuery_view (f k) db
    , _liveQuery_listen = \db p -> Vessel.traverseSubVessel $ \k -> _liveQuery_listen (f k) db p
    }

forEachTagInInnerVessel :: (GCompare tag, Has View tag, Ord k) => (forall v. tag v -> LiveQuery db (SubVessel k v)) -> LiveQuery db (SubVessel k (Vessel tag))
forEachTagInInnerVessel f = LiveQuery
  { _liveQuery_view = \db -> handleSubVesselSelector (\tag -> fmap getSubVessel . _liveQuery_view (f tag) db . mkSubVessel)
  , _liveQuery_listen = \db p -> handleSubVesselSelector (\tag -> fmap getSubVessel . _liveQuery_listen (f tag) db p . mkSubVessel)
  }

--TODO: Don't re-fetch except when necessary
-- | Provide a query that fetches items from a table by primary key.
--
-- This fetches items in bulk, but it currently re-fetches every changed item
-- whenever it is changed, even though this could be avoided sometimes in principle.
tableItems
  :: ( Ord (PrimaryKey t Identity)
     , FromBackendRow Postgres (PrimaryKey t Identity)
     , Database Postgres db
     , Table t
     , Ord (PrimaryKey t Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey t)
     , FromBackendRow Postgres (t Identity)
     , Ord (PrimaryKey t Identity)
     )
  => (forall f. db f -> f (TableEntity t))
  -> LiveQuery db (SubVessel (PrimaryKey t Identity) (SingleV (t Identity)))
tableItems getTable = LiveQuery
  { _liveQuery_view = \db ->
      fmap (mkSubVessel . fmap (SingleV . Identity . First))
    . getManyByPrimaryKeyWithMissing (getTable db)
    . MMap.keysSet
    . getSubVessel
  , _liveQuery_listen = \db (TablesV nm) ->
      fmap (mkSubVessel . fmap (SingleV . Identity . First))
    . liftNew
    . getManyByPrimaryKeyWithMissing (getTable db) --TODO: We can avoid a lot of lookups by reading more data out of the patch
    . Set.intersection (foldMap affectedKeys $ getComposeMaybe $ unTableOnly $ getTable nm)
    . MMap.keysSet
    . getSubVessel
  }

wholeTable
  :: ( Ord (PrimaryKey t Identity)
     , FromBackendRow Postgres (PrimaryKey t Identity)
     , Database Postgres db
     , Table t
     , Ord (PrimaryKey t Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey t)
     , FromBackendRow Postgres (t Identity)
     , Ord (PrimaryKey t Identity)
     )
  => (forall f. db f -> f (TableEntity t))
  -> LiveQuery db (IdentityV (SemiMap (PrimaryKey t Identity) (t Identity)))
wholeTable getTable = LiveQuery
  { _liveQuery_view = \db (IdentityV Proxy) -> do
      rows <- runSelectReturningList $ select $ all_ $ getTable db
      pure $ IdentityV $ Identity $ SemiMap_Complete $ MMap.fromList $ fmap (\r -> (pk r, r)) rows
  , _liveQuery_listen = \db (TablesV nm) (IdentityV Proxy) ->
      case getComposeMaybe $ unTableOnly $ getTable nm of
        Nothing -> pure mempty
        Just p -> do
          rows <- liftNew $ getManyByPrimaryKeyWithMissing (getTable db) $ affectedKeys p
          pure $ IdentityV $ Identity $ SemiMap_Partial $ fmap First rows
  }

affectedKeys :: TablePatch tbl -> Set (PrimaryKey tbl Identity)
affectedKeys = Map.keysSet . unPatchMapWithPatchingMove . unTablePatch

trackMissingLookupResults
  :: ( Functor f
     , Ord k
     )
  => (Set k -> f (MonoidalMap k v))
  -> Set k
  -> f (MonoidalMap k (Maybe v))
trackMissingLookupResults f ks = f ks <&> \present ->
  MMap.unionWith const (fmap Just present) $ MMap.fromSet (const Nothing) ks

getManyByPrimaryKeyWithMissing
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , Table t
     , Ord (PrimaryKey t Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey t)
     , FromBackendRow Postgres (t Identity)
     )
  => DatabaseEntity Postgres db (TableEntity t)
  -> Set (PrimaryKey t Identity)
  -> m (MonoidalMap (PrimaryKey t Identity) (Maybe (t Identity)))
getManyByPrimaryKeyWithMissing table = trackMissingLookupResults $ getManyByPrimaryKey table

getManyByPrimaryKey
  :: ( MonadBeam Postgres m
     , Database Postgres db
     , Table t
     , Ord (PrimaryKey t Identity)
     , FieldsFulfillConstraint (HasSqlValueSyntax PgValueSyntax) (PrimaryKey t)
     , FromBackendRow Postgres (t Identity)
     )
  => DatabaseEntity Postgres db (TableEntity t)
  -> Set (PrimaryKey t Identity)
  -> m (MonoidalMap (PrimaryKey t Identity) (t Identity))
getManyByPrimaryKey table itemIds = fmap (MMap.fromList . fmap (\b -> (pk b, b))) $ runSelectReturningList $ select $ do
  b <- all_ table
  guard_ $ in_ (primaryKey b) (fmap val_ $ toList itemIds)
  pure b