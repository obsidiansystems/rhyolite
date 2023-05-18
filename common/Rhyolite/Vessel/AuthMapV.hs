{-| Description: Authenticated vessel container
-}
{-# Language PolyKinds #-}
{-# Language LambdaCase #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language TypeApplications #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# Language ScopedTypeVariables #-}
{-# Language RankNTypes #-}

module Rhyolite.Vessel.AuthMapV where

import Data.Set (Set)
import Control.Monad.Writer.Strict (runWriterT) -- TODO: not strict enough, use writer-cps
import Control.Monad.Writer.Class (tell)
import Control.Monad.Trans.Class (lift)
import Data.These
import Data.Aeson
import Data.Constraint.Extras
import Data.Maybe
import qualified Data.Map.Monoidal as MMap
import qualified Data.Map.Strict as Map
import Data.Patch
import Data.Semigroup
import qualified Data.Set as Set
import Data.Vessel
import Data.Vessel.SubVessel
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism
import Data.Witherable
import GHC.Generics
import Reflex.Query.Class

import Rhyolite.App
import Rhyolite.Vessel.ErrorV

-- | A functor parametric container whose data is indexed by an authentication identity
-- where the result for each identity may succeed or fail.
newtype AuthMapV auth v g = AuthMapV { unAuthMapV :: SubVessel auth (ErrorV () v) g }
  deriving (Generic)

-- | Extract the authorised fragment of an 'AuthMapV'
getAuthMapV
  :: Ord auth
  => AuthMapV auth v g
  -> SubVessel auth v g
getAuthMapV (AuthMapV v) = mapMaybeWithKeySubVesselSlow (\_ -> snd . unsafeObserveErrorV) v

-- | Construct an authorised 'AuthMapV'
makeAuthMapV
  :: (Ord auth, View v)
  => SubVessel auth v g
  -> AuthMapV auth v g
makeAuthMapV = AuthMapV . mapMaybeWithKeySubVesselSlow (\_ -> Just . liftErrorV)

deriving instance (Ord auth, Eq (view g), Eq (g (First (Maybe ())))) => Eq (AuthMapV auth view g)

instance (Ord auth, ToJSON auth, ToJSON (g (First (Maybe ()))), ToJSON (view g)) => ToJSON (AuthMapV auth view g)
instance (Ord auth, FromJSON auth, View view, FromJSON (g (First (Maybe ()))), FromJSON (view g)) => FromJSON (AuthMapV auth view g)

deriving instance (Show (v f), Show (f (First (Maybe ()))), Show k) => Show (AuthMapV k v f)

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
  => (token -> Maybe user)
  -- ^ How to figure out the identity corresponding to a token. Note: this is pure because it absolutely must be cheap, and we don't want people
  -- attempting to put a database query inside it, which would result in terrible performance failures. Fast IO would be permissible, but generally
  -- decrypting a token with a known CSK can be done with a pure function.
  -> (v Proxy -> m (v Identity))
  -- ^ Handle the aggregate query for all identities
  -> AuthMapV token v Proxy
  -- ^ Private views parameterized by tokens
  -> m (AuthMapV token v Identity)
handleAuthMapQuery readToken handler (AuthMapV vt) = do
  let unfilteredVt = getSubVessel vt
      unvalidatedTokens = MMap.keys unfilteredVt
      validTokens = Set.fromList (filter (isJust . readToken) unvalidatedTokens)
  let filteredVt = MMap.intersectionWith const unfilteredVt (MMap.fromSet (\_ -> ()) validTokens)
      invalidTokens = MMap.fromSet (\_ -> failureErrorV ()) $
        Set.difference (Set.fromList unvalidatedTokens) validTokens
      v = condenseV filteredVt
  v' <- disperseV . fromMaybe emptyV <$> mapDecomposedV (buildErrorV (fmap Right . handler)) v
  -- The use of mapDecomposedV guarantees that the valid and invalid token sets are disjoint
  pure $ AuthMapV $ mkSubVessel $ MMap.unionWith const invalidTokens v'

newtype TaggedQuery a b = TaggedQuery { getTaggedQuery :: a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Monoid w => Applicative (TaggedQuery w) where
  pure _ = TaggedQuery mempty
  TaggedQuery f <*> TaggedQuery x = TaggedQuery (f <> x)

instance Monad (TaggedQuery ()) where
  TaggedQuery {} >>= _f = TaggedQuery ()

instance Semigroup w => Semigroup (TaggedQuery w x) where
  TaggedQuery a <> TaggedQuery b = TaggedQuery (a <> b)

instance Monoid w => Monoid (TaggedQuery w x) where
  mempty = TaggedQuery mempty

type instance ViewQueryResult (TaggedQuery w) = (,) w
type instance ViewQueryResult (TaggedQuery w a) = (w, a)

-- | Like 'handleAuthMapQuery' but the result can depend on the specific identity.
-- This is implemented naively so that the query is done separately for each valid identity.
handlePersonalAuthMapQuery
  :: forall m token v user.
     (Monad m, Ord token, View v, Ord user)
  => (token -> Maybe user)
  -- ^ How to figure out the identity corresponding to a token. Note: this is pure because it absolutely must be cheap. See the corresponding comment on
  -- 'handleAuthMapQuery'.
  -> (forall f g.
     ViewQueryResult f ~ g
     => (forall x. x -> f x -> g x)
     -> v (Compose (MMap.MonoidalMap user) f)
     -> m (v (Compose (MMap.MonoidalMap user) g))
     )
  -- ^ Handle the query for each individual identity
  -> AuthMapV token v Proxy
  -- ^ Personal views parameterized by tokens
  -> m (AuthMapV token v Identity)
handlePersonalAuthMapQuery readToken handler vt = do
  let unauthorisedAuthMapSingleton token = Map.singleton token $ failureErrorV ()

      authoriseAction t v =
        case readToken t of
          Nothing -> do
            tell $ unauthorisedAuthMapSingleton t
            pure Nothing
          Just u' -> pure $ Just $ mapV (Compose . (MMap.singleton u')) v

      condenseTokens
        :: Compose (MMap.MonoidalMap token) (Compose (MMap.MonoidalMap user) Proxy) a
        -> Compose (MMap.MonoidalMap user) (TaggedQuery (Set token)) a
      condenseTokens =
        Compose
        . (MMap.foldMapWithKey $ \t (Compose u) -> TaggedQuery (Set.singleton t) <$ u)
        . getCompose

      injectResult :: forall x.  x -> TaggedQuery (Set token) x -> ((Set token), x)
      injectResult x (TaggedQuery xs) = (xs, x)

      disperseTokens
        :: MMap.MonoidalMap user (Set token, a)
        -> Compose (MMap.MonoidalMap token) Identity a
      disperseTokens = Compose . MMap.MonoidalMap
        . foldMap (\(t, a) -> Map.fromSet (const (Identity a)) t)

  (vtReadToken, invalidTokens) <- runWriterT $ traverseMaybeSubVesselSlow authoriseAction $ getAuthMapV vt

  vt' <- handler injectResult $ mapV condenseTokens $ condenseV $ getSubVessel vtReadToken

  -- TODO: warn about collisions in alignWithV
  pure $ alignWithV (these id id const) (AuthMapV $ mkSubVessel $ MMap.MonoidalMap invalidTokens)
      (makeAuthMapV (mkSubVessel $ disperseV $ mapV (disperseTokens . getCompose) vt'))

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

