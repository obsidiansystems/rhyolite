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
data AuthenticatedVKey public private (x :: (* -> *) -> *) where
  AuthenticatedVKey_Public :: AuthenticatedVKey public private public
  AuthenticatedVKey_Private :: AuthenticatedVKey public private private

deriveJSONGADT ''AuthenticatedVKey

instance GEq (AuthenticatedVKey public private) where
  geq = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> Just Refl
      AuthenticatedVKey_Private -> Nothing
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> Nothing
      AuthenticatedVKey_Private -> Just Refl

instance GCompare (AuthenticatedVKey public private) where
  gcompare = \case
    AuthenticatedVKey_Public -> \case
      AuthenticatedVKey_Public -> GEQ
      AuthenticatedVKey_Private -> GLT
    AuthenticatedVKey_Private -> \case
      AuthenticatedVKey_Public -> GGT
      AuthenticatedVKey_Private -> GEQ

instance ArgDict c (AuthenticatedVKey public private) where
  type ConstraintsFor (AuthenticatedVKey public private) c = (c public, c private)
  argDict = \case
    AuthenticatedVKey_Public -> Dict
    AuthenticatedVKey_Private -> Dict

-- | A functor-parametric container that has a public part and a private part.
newtype AuthenticatedV public private g = AuthenticatedV
  { unAuthenticatedV :: Vessel (AuthenticatedVKey public private) g
  } deriving (Generic, Eq, ToJSON, FromJSON, Semigroup, Monoid, Group, Additive)

instance (View public, View private) => View (AuthenticatedV public private)

instance (View public, View private) => EmptyView (AuthenticatedV public private) where
  emptyV = AuthenticatedV emptyV

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private Proxy) ~ private Identity
  ) => Query (AuthenticatedV public private Proxy) where
  type QueryResult (AuthenticatedV public private Proxy) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private (Const ())) ~ private Identity
  ) => Query (AuthenticatedV public private (Const ())) where
  type QueryResult (AuthenticatedV public private (Const ())) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( Semigroup (public Identity)
  , Semigroup (private Identity)
  , View public, View private
  , QueryResult (private (Const SelectedCount)) ~ private Identity
  ) => Query (AuthenticatedV public private (Const SelectedCount)) where
  type QueryResult (AuthenticatedV public private (Const SelectedCount)) = AuthenticatedV public private Identity
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

instance
  ( View public, View private
  , Semigroup (private (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g)))))
  , Semigroup (public (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g)))))
  , Query (Vessel (AuthenticatedVKey public private) g)
  ) => Query (AuthenticatedV public private (Compose c (g :: * -> *))) where
  type QueryResult (AuthenticatedV public private (Compose c g)) = AuthenticatedV public private
         (Compose c (VesselLeafWrapper (QueryResult (Vessel (AuthenticatedVKey public private) g))))
  crop (AuthenticatedV s) (AuthenticatedV r) = AuthenticatedV $ crop s r

-- | Given a handler for each partial view container, produces
-- a handler for the total view container.
handleAuthenticatedQuery'
  :: (Monad m, View public, View private)
  => (public Proxy -> m (public Identity))
  -- ^ Handle the aggregate public query
  -> (private Proxy -> m (private Identity))
  -- ^ Handle the aggregate private query for all identities
  -> AuthenticatedV public private Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthenticatedV public private Identity)
handleAuthenticatedQuery' public private (AuthenticatedV q) = fmap AuthenticatedV $ buildV q $ \case
  AuthenticatedVKey_Public -> public
  AuthenticatedVKey_Private -> private

-- | Very frequently the private part of a total view container is
-- a map from authentication identities to private views. This
-- handler bakes this assumption in.
handleAuthenticatedQuery
  :: (Monad m, Ord token, View public, View private)
  => (token -> m (Maybe user))
  -> (public Proxy -> m (public Identity))
  -> (private Proxy -> m (private Identity))
  -> AuthenticatedV public (AuthMapV token private) Proxy
  -> m (AuthenticatedV public (AuthMapV token private) Identity)
handleAuthenticatedQuery readToken public private =
  handleAuthenticatedQuery' public (handleAuthMapQuery readToken private)

-- | Ignore the public part of a total view container.
privateQueryMorphism
  :: ( EmptyView private, QueryResult (private (Const SelectedCount)) ~ private Identity
     )
  => QueryMorphism
       (private (Const SelectedCount))
       (AuthenticatedV public private (Const SelectedCount))
privateQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Private
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Private . unAuthenticatedV
  }

-- | Ignore the private part of a total view container.
publicQueryMorphism
  :: ( EmptyView public, QueryResult (public (Const SelectedCount)) ~ public Identity
     )
  => QueryMorphism
       (public (Const SelectedCount))
       (AuthenticatedV public private (Const SelectedCount))
publicQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = AuthenticatedV . singletonV AuthenticatedVKey_Public
  , _queryMorphism_mapQueryResult = maybe emptyV id . lookupV AuthenticatedVKey_Public . unAuthenticatedV
  }
