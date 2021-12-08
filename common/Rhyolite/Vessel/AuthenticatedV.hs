{-| Description: Authenticated views -}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
module Rhyolite.Vessel.AuthenticatedV where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint
import Data.Constraint.Extras
import Data.GADT.Compare
import Data.Patch
import Data.Type.Equality
import Data.Vessel
import Data.Vessel.Vessel
import GHC.Generics
import Reflex.Query.Class

import Rhyolite.Vessel.AuthMapV

-- | An internal key type used to glue together parts of a view selector
-- that have different authentication contexts.
data AuthenticatedVKey public private personal (ix :: (* -> *) -> *) where
  AuthenticatedVKey_Public :: AuthenticatedVKey public private personal public
  AuthenticatedVKey_Private :: AuthenticatedVKey public private personal private
  AuthenticatedVKey_Personal :: AuthenticatedVKey public private personal personal

deriveJSONGADT ''AuthenticatedVKey

instance GEq (AuthenticatedVKey public private personal) where
  geq = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> Just Refl
      AuthenticatedVKey_Private -> Nothing
      AuthenticatedVKey_Personal -> Nothing
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> Nothing
      AuthenticatedVKey_Private -> Just Refl
      AuthenticatedVKey_Personal -> Nothing
    AuthenticatedVKey_Personal -> \case
      AuthenticatedVKey_Public -> Nothing
      AuthenticatedVKey_Private -> Nothing
      AuthenticatedVKey_Personal -> Just Refl

instance GCompare (AuthenticatedVKey public private personal) where
  gcompare = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> GEQ
      AuthenticatedVKey_Private -> GLT
      AuthenticatedVKey_Personal -> GLT
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> GGT
      AuthenticatedVKey_Private -> GEQ
      AuthenticatedVKey_Personal -> GLT
    AuthenticatedVKey_Personal -> \case
      AuthenticatedVKey_Public -> GGT
      AuthenticatedVKey_Private -> GGT
      AuthenticatedVKey_Personal -> GEQ

instance ArgDict c (AuthenticatedVKey public private personal) where
  type ConstraintsFor (AuthenticatedVKey public private personal) c = (c public, c private, c personal)
  argDict = \case
    AuthenticatedVKey_Public -> Dict
    AuthenticatedVKey_Private -> Dict
    AuthenticatedVKey_Personal -> Dict

-- | A functor-parametric container that has a public part and a private part.
newtype AuthenticatedV public private personal g = AuthenticatedV
  { unAuthenticatedV :: Vessel (AuthenticatedVKey public private personal) g
  } deriving (Generic, Eq, ToJSON, FromJSON, Semigroup, Monoid, Group, Additive)

instance (View public, View private, View personal) => View (AuthenticatedV public private personal)

instance (View public, View private, View personal) => EmptyView (AuthenticatedV public private personal) where
  emptyV = AuthenticatedV emptyV

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , Semigroup (personal Identity)
  , View public, View private, View personal
  ) => Query (AuthenticatedV public private personal Proxy) where
  type QueryResult (AuthenticatedV public private personal Proxy) = AuthenticatedV public private personal Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , Semigroup (personal Identity)
  , View public, View private, View personal
  ) => Query (AuthenticatedV public private personal (Const ())) where
  type QueryResult (AuthenticatedV public private personal (Const ())) = AuthenticatedV public private personal Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , Semigroup (personal Identity)
  , View public, View private, View personal
  ) => Query (AuthenticatedV public private personal (Const SelectedCount)) where
  type QueryResult (AuthenticatedV public private personal (Const SelectedCount)) = AuthenticatedV public private personal Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( View public, View private, View personal
  , Semigroup (private (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private personal) g)))))
  , Semigroup (public (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private personal) g)))))
  , Semigroup (personal (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private personal) g)))))
  , Query (Vessel (AuthenticatedVKey public private personal) g)
  ) => Query (AuthenticatedV public private personal (Compose c (g :: * -> *))) where
  type QueryResult (AuthenticatedV public private personal (Compose c g)) = AuthenticatedV public private personal
         (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private personal) g))))
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

-- | Given a handler for each partial view container, produces
-- a handler for the total view container.
handleAuthenticatedQuery'
  :: (Monad m, View public, View private, View personal)
  => (public Proxy -> m (public Identity))
  -- ^ Handle the aggregate public query
  -> (private Proxy -> m (private Identity))
  -- ^ Handle the aggregate private query for all identities
  -> (personal Proxy -> m (personal Identity))
  -> AuthenticatedV public private personal Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthenticatedV public private personal Identity)
handleAuthenticatedQuery' public private personal (AuthenticatedV q) = fmap AuthenticatedV $ buildV q $ \case
  AuthenticatedVKey_Public -> public
  AuthenticatedVKey_Private -> private
  AuthenticatedVKey_Personal -> personal

-- | Very frequently the private part of a total view container is
-- a map from authentication identities to private views. This
-- handler bakes this assumption in.
handleAuthenticatedQuery
  :: (Monad m, Ord token, View public, View private, View personal)
  => (token -> m (Maybe user))
  -> (public Proxy -> m (public Identity))
  -> (private Proxy -> m (private Identity))
  -- ^ The result of private queries is only available to authenticated identities
  -- but the result is the same for all of them.
  -> (user -> personal Proxy -> m (personal Identity))
  -- ^ The result of personal queries depends on the identity making the query
  -> AuthenticatedV public (AuthMapV token private) (AuthMapV token personal) Proxy
  -> m (AuthenticatedV public (AuthMapV token private) (AuthMapV token personal) Identity)
handleAuthenticatedQuery readToken public private personal =
  handleAuthenticatedQuery'
    public
    (handleAuthMapQuery readToken private)
    (handlePersonalAuthMapQuery readToken personal)

-- | Ignore the public part of a total view container.
privateQueryMorphism
  :: ( EmptyView private, QueryResult (private (Const SelectedCount)) ~ private Identity
     )
  => QueryMorphism
       (private (Const SelectedCount))
       (AuthenticatedV public private personal (Const SelectedCount))
privateQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Private
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Private . unAuthenticatedV
  }

-- | Ignore the public part of a total view container.
personalQueryMorphism
  :: ( EmptyView personal, QueryResult (personal (Const SelectedCount)) ~ personal Identity
     )
  => QueryMorphism
       (personal (Const SelectedCount))
       (AuthenticatedV public private personal (Const SelectedCount))
personalQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Personal
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Personal . unAuthenticatedV
  }

-- | Ignore the private part of a total view container.
publicQueryMorphism
  :: ( EmptyView public, QueryResult (public (Const SelectedCount)) ~ public Identity
     )
  => QueryMorphism
       (public (Const SelectedCount))
       (AuthenticatedV public private personal (Const SelectedCount))
publicQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Public
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Public . unAuthenticatedV
  }
