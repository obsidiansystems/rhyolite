{-| Description: ErrorV implementation
-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
module Rhyolite.Vessel.ErrorV.Internal where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint
import Data.Constraint.Extras
import Data.GADT.Compare
import Data.Patch
import Data.Semigroup
import Data.Type.Equality
import Data.Vessel
import Data.Vessel.Single
import Data.Vessel.Vessel
import GHC.Generics
import Reflex.Query.Class

import Rhyolite.App

-- | An internal type which specifies the two pieces of an 'ErrorV' container.
-- When constructing a query, only the View piece is present. When constructing
-- a result, either the Error or View part will be present but never both.
data ErrorVK err view (v :: (* -> *) -> *) where
  ErrorVK_Error :: ErrorVK err view (SingleV err)
  ErrorVK_View :: ErrorVK err view view

deriveJSONGADT ''ErrorVK

instance GEq (ErrorVK err view) where
  geq = \case
    ErrorVK_Error -> \case
      ErrorVK_Error -> Just Refl
      ErrorVK_View -> Nothing
    ErrorVK_View -> \case
      ErrorVK_Error -> Nothing
      ErrorVK_View -> Just Refl

instance GCompare (ErrorVK err view) where
  gcompare = \case
    ErrorVK_Error -> \case
      ErrorVK_Error -> GEQ
      ErrorVK_View -> GLT
    ErrorVK_View -> \case
      ErrorVK_Error -> GGT
      ErrorVK_View -> GEQ

instance ArgDict c (ErrorVK err view) where
  type ConstraintsFor (ErrorVK err view) c = (c (SingleV err), c view)
  argDict = \case
    ErrorVK_Error -> Dict
    ErrorVK_View -> Dict

-- | A functor-parametric container which as a query will contain a value of the
-- underlying view type and as a result may contain either an err value or a
-- view result value.
newtype ErrorV err view g = ErrorV { unErrorV :: Vessel (ErrorVK err view) g }
  deriving (Generic, EmptyView)

deriving instance (Eq (g (First (Maybe err))), Eq (view g)) => Eq (ErrorV err view g)

instance View view => View (ErrorV err view)

instance (ToJSON (g (First (Maybe err))), ToJSON (view g)) => ToJSON (ErrorV err view g)
instance (View view, FromJSON (g (First (Maybe err))), FromJSON (view g)) => FromJSON (ErrorV err view g)

deriving instance (Has' Semigroup (ErrorVK err v) (FlipAp g), View v) => Semigroup (ErrorV err v g)
deriving instance (Has' Semigroup (ErrorVK err v) (FlipAp g), View v) => Monoid (ErrorV err v g)
deriving instance (Has' Additive (ErrorVK err v) (FlipAp g), View v) => Additive (ErrorV err v g)
deriving instance (Has' Group (ErrorVK err v) (FlipAp g), View v) => Group (ErrorV err v g)
deriving instance (PositivePart (g (First (Maybe err))), PositivePart (v g)) => PositivePart (ErrorV err v g)

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v Proxy) ~ v Identity
  ) => Query (ErrorV err v Proxy) where
  type QueryResult (ErrorV err v Proxy) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v (Const ())) ~ v Identity
  ) => Query (ErrorV err v (Const ())) where
  type QueryResult (ErrorV err v (Const ())) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( Semigroup (v Identity)
  , View v
  , QueryResult (v (Const SelectedCount)) ~ v Identity
  ) => Query (ErrorV err v (Const SelectedCount)) where
  type QueryResult (ErrorV err v (Const SelectedCount)) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( View v
  , Has' Semigroup (ErrorVK err v) (FlipAp (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g)))))
  , Query (Vessel (ErrorVK err v) g)
  ) => Query (ErrorV err v (Compose c g)) where
  type QueryResult (ErrorV err v (Compose c g)) = ErrorV err v (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g))))
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

-- | The error part of the view will never be present
liftErrorV :: View v => v g -> ErrorV e v g
liftErrorV = ErrorV . singletonV ErrorVK_View

-- | The successful part of the view will never be present
failureErrorV :: e -> ErrorV e v Identity
failureErrorV = ErrorV . singletonV ErrorVK_Error . SingleV . Identity . First . Just

-- | Given an 'ErrorV' query and a way to provide a possibly failing result,
-- construct an ErrorV result.
buildErrorV
  :: (View v, Monad m)
  => (v Proxy -> m (Either e (v Identity)))
  -> ErrorV e v Proxy
  -> m (ErrorV e v Identity)
buildErrorV f (ErrorV v) = case lookupV ErrorVK_View v of
  Nothing -> pure (ErrorV emptyV)
  Just v' -> f v' >>= \case
    Left err -> pure $ failureErrorV err
    Right val -> pure $ liftErrorV val

-- | Given an 'ErrorV' result, observe whether it is an error result
-- or a result of the underlying view type.
observeErrorV
  :: EmptyView v
  => ErrorV e v Identity
  -> Either e (v Identity)
observeErrorV (ErrorV v) = case lookupV ErrorVK_Error v of
  Nothing -> Right $ case lookupV ErrorVK_View v of
    Nothing -> emptyV
    Just v' -> v'
  Just err -> case lookupSingleV err of
    Nothing -> Right emptyV
    Just e -> Left e

-- | A morphism that only cares about error results.
unsafeProjectE
  :: ( EmptyView v
     )
  => QueryMorphism
       ()
       (ErrorV () v (Const SelectedCount))
unsafeProjectE = QueryMorphism
  { _queryMorphism_mapQuery = const (liftErrorV emptyV)
  , _queryMorphism_mapQueryResult = const ()
  }

-- | A morphism that only cares about successful results.
unsafeProjectV
  :: (EmptyView v, QueryResult (v (Const SelectedCount)) ~ v Identity)
  => QueryMorphism
       (v (Const SelectedCount))
       (ErrorV () v (Const SelectedCount))
unsafeProjectV = QueryMorphism
  { _queryMorphism_mapQuery = liftErrorV
  , _queryMorphism_mapQueryResult = \r -> case observeErrorV r of
      Left _ -> emptyV
      Right r' -> r'
  }
