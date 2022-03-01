{-| Description: Authenticated views -}
{-# Language DeriveGeneric #-}
{-# Language TypeApplications #-}
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
{-# Language StandaloneDeriving #-}
-- TODO Upstream for DMap
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Rhyolite.Vessel.AuthenticatedV where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint
import Data.Constraint.Extras
import Data.GADT.Compare
import Data.GADT.Show
import Data.Patch
import Data.Type.Equality
import Data.Vessel
import Data.Vessel.Vessel
import Data.Semigroup
import Data.Vessel.Path (Keyed(..))
import GHC.Generics
import Reflex.Query.Class
import Data.Map.Monoidal (MonoidalMap)

-- TODO Upstream a function that lets you change DMap keys monotonically
-- while mutating the value so we don't have to do it here.
import Data.Dependent.Map.Monoidal (MonoidalDMap(..))
import Data.Dependent.Map.Internal (DMap)
import qualified Data.Dependent.Map.Internal as DMap
import Data.Dependent.Sum

import Rhyolite.App
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.ErrorV

import Control.Applicative (Alternative)
import Prelude hiding ((.), id)
import Control.Category
import Data.Vessel.ViewMorphism (ViewQueryResult, ViewMorphism(..), ViewHalfMorphism(..))
import Data.Vessel.Vessel (vessel)
import Data.Bifoldable


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
  } deriving (Generic, Eq, ToJSON, FromJSON, Semigroup, Monoid, Group, Additive, PositivePart)

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
  ) => Query (AuthenticatedV public private personal (Const g)) where
  type QueryResult (AuthenticatedV public private personal (Const g)) = AuthenticatedV public private personal Identity
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
  :: (Monad m, Ord token, View public, View private, View personal, Ord user)
  => (token -> m (Maybe user))
  -> (public Proxy -> m (public Identity))
  -> (private Proxy -> m (private Identity))
  -- ^ The result of private queries is only available to authenticated identities
  -- but the result is the same for all of them.
  -> (forall f g. (forall x. x -> f x -> g x)
      -> personal (Compose (MonoidalMap user) f)
      -> m (personal (Compose (MonoidalMap user) g)))
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

-- | Apply query morphisms to the constituents of an 'AuthenticatedV'
mapAuthenticatedV
  :: forall public public' private private' personal personal'.
     ( QueryResult (public (Const SelectedCount)) ~ public Identity
     , QueryResult (public' (Const SelectedCount)) ~ public' Identity
     , QueryResult (private (Const SelectedCount)) ~ private Identity
     , QueryResult (private' (Const SelectedCount)) ~ private' Identity
     , QueryResult (personal (Const SelectedCount)) ~ personal Identity
     , QueryResult (personal' (Const SelectedCount)) ~ personal' Identity
     )
  => QueryMorphism (public (Const SelectedCount)) (public' (Const SelectedCount))
  -> QueryMorphism (private (Const SelectedCount)) (private' (Const SelectedCount))
  -> QueryMorphism (personal (Const SelectedCount)) (personal' (Const SelectedCount))
  -> QueryMorphism
       (AuthenticatedV public private personal (Const SelectedCount))
       (AuthenticatedV public' private' personal' (Const SelectedCount))
mapAuthenticatedV publicMorphism privateMorphism personalMorphism = QueryMorphism
  { _queryMorphism_mapQuery = \(AuthenticatedV (Vessel (MonoidalDMap rawDMap))) ->
    AuthenticatedV $ Vessel $ MonoidalDMap $ mapKeysMonotonic' queryMap rawDMap
  , _queryMorphism_mapQueryResult = \(AuthenticatedV (Vessel (MonoidalDMap rawDMap))) ->
    AuthenticatedV $ Vessel $ MonoidalDMap $ mapKeysMonotonic' resultMap rawDMap
  }
 where
  queryMap
    :: forall ix.
       AuthenticatedVKey public private personal ix
    -> FlipAp (Const SelectedCount) ix
    -> DSum (AuthenticatedVKey public' private' personal') (FlipAp (Const SelectedCount))
  queryMap k v = case k of
    AuthenticatedVKey_Public ->
      AuthenticatedVKey_Public :=> FlipAp (mapQuery publicMorphism (unFlipAp v))
    AuthenticatedVKey_Private ->
      AuthenticatedVKey_Private :=> FlipAp (mapQuery privateMorphism (unFlipAp v))
    AuthenticatedVKey_Personal ->
      AuthenticatedVKey_Personal :=> FlipAp (mapQuery personalMorphism (unFlipAp v))
  resultMap
    :: forall ix.
       AuthenticatedVKey public' private' personal' ix
    -> FlipAp Identity ix
    -> DSum (AuthenticatedVKey public private personal) (FlipAp Identity)
  resultMap k v = case k of
    AuthenticatedVKey_Public ->
      AuthenticatedVKey_Public :=> FlipAp (mapQueryResult publicMorphism (unFlipAp v))
    AuthenticatedVKey_Private ->
      AuthenticatedVKey_Private :=> FlipAp (mapQueryResult privateMorphism (unFlipAp v))
    AuthenticatedVKey_Personal ->
      AuthenticatedVKey_Personal :=> FlipAp (mapQueryResult personalMorphism (unFlipAp v))

mapKeysMonotonic'
  :: (forall v. k1 v -> f v -> (DSum k2 g))
  -> DMap k1 f
  -> DMap k2 g
mapKeysMonotonic' f = \case
  DMap.Tip -> DMap.Tip
  DMap.Bin n k v bl br ->
    case f k v of
      (k' :=> v') -> DMap.Bin n k' v'
        (mapKeysMonotonic' f bl)
        (mapKeysMonotonic' f br)

traverseKeysMonotonic'
  :: forall m k1 f k2 g. Applicative m
  => (forall v. k1 v -> f v -> m (DSum k2 g))
  -> DMap k1 f
  -> m (DMap k2 g)
traverseKeysMonotonic' f = go
  where
    go :: DMap k1 f -> m (DMap k2 g)
    go = \case
      DMap.Tip -> pure DMap.Tip
      DMap.Bin n k v bl br ->
        (\(k' :=> v') -> DMap.Bin n k' v')
          <$> (f k v)
          <*> (go bl)
          <*> (go br)

type instance ViewQueryResult (AuthenticatedV public private personal g) = AuthenticatedV public private personal (ViewQueryResult g)

instance View v => Keyed
    (AuthenticatedVKey public private personal v)
    (v g)
    (AuthenticatedV public private personal g)
    (AuthenticatedV public private personal g')
    (v g') where
  key k = Path (AuthenticatedV . singletonV k) (lookupV k . unAuthenticatedV)

authenticatedV
  :: ( Monad m, Monad n, Alternative n
     , View v, ViewQueryResult (v g) ~ v (ViewQueryResult g)
     )
  => AuthenticatedVKey public private personal v
  -> ViewMorphism m n (v g) (AuthenticatedV public private personal g)
authenticatedV k = ViewMorphism
  (ViewHalfMorphism (pure . AuthenticatedV) (pure . unAuthenticatedV))
  (ViewHalfMorphism (pure . unAuthenticatedV) (pure . AuthenticatedV))
  . vessel k


publicP :: forall public private personal g g'. View public
  => Path
    (public g)
    (AuthenticatedV public private personal g)
    (AuthenticatedV public private personal g')
    (public g')
publicP = key (AuthenticatedVKey_Public @public @private @personal)

privateP :: forall public private personal g g'. View private
  => Path
    (private g)
    (AuthenticatedV public private personal g)
    (AuthenticatedV public private personal g')
    (private g')
privateP = key (AuthenticatedVKey_Private @public @private @personal)

personalP :: forall public private personal g g'. View personal
  => Path
    (personal g)
    (AuthenticatedV public private personal g)
    (AuthenticatedV public private personal g')
    (personal g')
personalP = key (AuthenticatedVKey_Personal @public @private @personal)

publicV :: forall m n public private personal g.
  (ViewQueryResult (public g) ~ public (ViewQueryResult g), Monad m, Monad n, Alternative n, View public)
  => ViewMorphism m n (public g) (AuthenticatedV public private personal g)
publicV = authenticatedV (AuthenticatedVKey_Public @public @private @personal)

privateV :: forall m n public private personal g.
  (ViewQueryResult (private g) ~ private (ViewQueryResult g), Monad m, Monad n, Alternative n, View private)
  => ViewMorphism m n (private g) (AuthenticatedV public private personal g)
privateV = authenticatedV (AuthenticatedVKey_Private @public @private @personal)

personalV :: forall m n public private personal g.
  (ViewQueryResult (personal g) ~ personal (ViewQueryResult g), Monad m, Monad n, Alternative n, View personal)
  => ViewMorphism m n (personal g) (AuthenticatedV public private personal g)
personalV = authenticatedV (AuthenticatedVKey_Personal @public @private @personal)

traverseAuthenticatedV ::
  forall m public private personal public' private' personal' f g.
  (Applicative m)
  => (public f -> m (public' g))
  -> (private f -> m (private' g))
  -> (personal f -> m (personal' g))
  -> AuthenticatedV public private personal f
  -> m (AuthenticatedV public' private' personal' g)
traverseAuthenticatedV f g h (AuthenticatedV (Vessel (MonoidalDMap v))) = AuthenticatedV . Vessel . MonoidalDMap <$> traverseKeysMonotonic' go v
  where
    go :: forall v. AuthenticatedVKey public private personal v
             -> FlipAp f v
             -> m (DSum
                     (AuthenticatedVKey public' private' personal') (FlipAp g))
    go = \case
      AuthenticatedVKey_Public -> \(FlipAp x) -> (\y -> AuthenticatedVKey_Public :=> FlipAp y) <$> f x
      AuthenticatedVKey_Private -> \(FlipAp x) -> (\y -> AuthenticatedVKey_Private :=> FlipAp y) <$> g x
      AuthenticatedVKey_Personal -> \(FlipAp x) -> (\y -> AuthenticatedVKey_Personal :=> FlipAp y) <$> h x

disperseAuthenticatedErrorV ::
  ( View publicV , Semigroup (publicV Identity)
  , EmptyView privateV , Semigroup (privateV Identity)
  , EmptyView personalV , Semigroup (personalV Identity)
  )
  => QueryMorphism
    (ErrorV () (AuthenticatedV publicV privateV personalV) (Const x))
    (AuthenticatedV publicV (ErrorV () privateV) (ErrorV () personalV) (Const x))
disperseAuthenticatedErrorV = QueryMorphism
  (maybe emptyV (runIdentity . traverseAuthenticatedV
      pure
      (pure . liftErrorV)
      (pure . liftErrorV))
    . snd . unsafeObserveErrorV)
  (bifoldMap @(,) (maybe emptyV failureErrorV . (=<<) (getFirst . runIdentity)) liftErrorV
    . traverseAuthenticatedV
      ((,) Nothing)
      (fmap (maybe emptyV id) . unsafeObserveErrorV)
      (fmap (maybe emptyV id) . unsafeObserveErrorV)
      )

deriving instance Show (AuthenticatedVKey publicV privateV personalV v)
instance GShow (AuthenticatedVKey publicV privateV personalV) where
  gshowsPrec = showsPrec

deriving instance
    ( Show (publicV f)
    , Show (privateV f)
    , Show (personalV f)
    )
    =>
    Show (AuthenticatedV publicV privateV personalV f)
