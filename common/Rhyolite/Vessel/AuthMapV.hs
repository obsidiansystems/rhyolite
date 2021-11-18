{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Rhyolite.Vessel.AuthMapV where

import Data.Aeson
import Data.Constraint.Extras
import Data.Maybe
import qualified Data.Map.Monoidal as MMap
import Data.Patch
import Data.Semigroup
import qualified Data.Set as Set
import Data.Vessel
import Data.Vessel.SubVessel
import Data.Vessel.Vessel
import Data.Witherable
import GHC.Generics
import Reflex.Query.Class

import Rhyolite.App
import Rhyolite.Vessel.ErrorV

-- | A functor parametric container whose data is indexed by an authentication identity
-- where the result for each identity may succeed or fail.
newtype AuthMapV auth v g = AuthMapV { unAuthMapV :: SubVessel auth (ErrorV () v) g }
  deriving (Generic)

deriving instance (Ord auth, Eq (view g), Eq (g (First (Maybe ())))) => Eq (AuthMapV auth view g)

instance (Ord auth, ToJSON auth, ToJSON (g (First (Maybe ()))), ToJSON (view g)) => ToJSON (AuthMapV auth view g)
instance (Ord auth, FromJSON auth, View view, FromJSON (g (First (Maybe ()))), FromJSON (view g)) => FromJSON (AuthMapV auth view g)

deriving instance
  ( Ord auth
  , Has' Semigroup (ErrorVK () v) (FlipAp g)
  , View v
  ) => Semigroup (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Semigroup (ErrorVK () v) (FlipAp g)
  , View v
  ) => Monoid (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Group (ErrorVK () v) (FlipAp g)
  , View v
  ) => Group (AuthMapV auth v g)

deriving instance
  ( Ord auth
  , Has' Additive (ErrorVK () v) (FlipAp g)
  , View v
  ) => Additive (AuthMapV auth v g)

deriving instance (Ord auth, PositivePart (g (First (Maybe ()))), PositivePart (v g)) => PositivePart (AuthMapV auth v g)

instance (Ord auth, View v) => View (AuthMapV auth v)
instance (Ord auth, View v) => EmptyView (AuthMapV auth v) where
  emptyV = AuthMapV emptyV

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v Proxy) ~ v Identity
  ) => Query (AuthMapV auth v Proxy) where
  type QueryResult (AuthMapV auth v Proxy) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v (Const ())) ~ v Identity
  ) => Query (AuthMapV auth v (Const ())) where
  type QueryResult (AuthMapV auth v (Const ())) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , Semigroup (v Identity)
  , View v
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  ) => Query (AuthMapV auth v (Const SelectedCount)) where
  type QueryResult (AuthMapV auth v (Const SelectedCount)) = AuthMapV auth v Identity
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

instance
  ( Ord auth
  , View v
  , Has' Semigroup (ErrorVK () v) (FlipAp (Compose c (VesselLeafWrapper (QueryResult (Vessel (SubVesselKey auth (ErrorV () v)) g)))))
  , Query (Vessel (SubVesselKey auth (ErrorV () v)) g)
  ) => Query (AuthMapV auth v (Compose c g)) where
  type QueryResult (AuthMapV auth v (Compose c g)) = AuthMapV auth v (Compose c (VesselLeafWrapper (QueryResult (Vessel (SubVesselKey auth (ErrorV () v)) g))))
  crop (AuthMapV s) (AuthMapV r) = AuthMapV $ crop s r

-- | Given a way to verify that a token corresponds to a valid identity and
-- a way to handle queries whose result does not depend on the particular
-- identity making the query, handles each query efficiently while
-- distributing the results only to valid identities.
--
-- token is typically like 'Signed (AuthToken Identity)'
-- user is typically 'Id Account', iso to 'AuthToken Identity'
handleAuthMapQuery
  :: (Monad m, Ord token, View v)
  => (token -> m (Maybe user))
  -- ^ How to figure out the identity corresponding to a token
  -> (v Proxy -> m (v Identity))
  -- ^ Handle the aggregate query for all identities
  -> AuthMapV token v Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthMapV token v Identity)
handleAuthMapQuery readToken handler (AuthMapV vt) = do
  let unfilteredVt = getSubVessel vt
      unvalidatedTokens = MMap.keys unfilteredVt
  validTokens <- Set.fromList <$> witherM (\t -> (t <$) <$> readToken t) unvalidatedTokens
  let filteredVt = MMap.intersectionWith const unfilteredVt (MMap.fromSet (\_ -> ()) validTokens)
      invalidTokens = MMap.fromSet (\_ -> failureErrorV ()) $
        Set.difference (Set.fromList unvalidatedTokens) validTokens
      v = condenseV filteredVt
  v' <- disperseV . fromMaybe emptyV <$> mapDecomposedV (buildErrorV (fmap Right . handler)) v
  -- The use of mapDecomposedV guarantees that the valid and invalid token sets are disjoint
  pure $ AuthMapV $ mkSubVessel $ MMap.unionWith const invalidTokens v'

-- | A query morphism that takes a view for a single identity and lifts it to
-- a map of identities to views.
authMapQueryMorphism
  :: (Ord token, View v)
  => token
  -> QueryMorphism
       (ErrorV () v (Const SelectedCount))
       (AuthMapV token v (Const SelectedCount))
authMapQueryMorphism token = QueryMorphism
  { _queryMorphism_mapQuery = AuthMapV . singletonSubVessel token
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupSubVessel token . unAuthMapV
  }

