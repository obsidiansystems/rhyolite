{-| Description: ErrorV implementation
-}
{-# Language ConstraintKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}
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
import Data.GADT.Show
import Data.Orphans ()
import Data.Patch
import Data.Semigroup
import Data.Semigroup.Commutative
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

instance (c (SingleV err), c view) => Has c (ErrorVK err view) where
  argDict = \case
    ErrorVK_Error -> Dict
    ErrorVK_View -> Dict

deriving instance Show (ErrorVK e v a)

instance GShow (ErrorVK e v) where
  gshowsPrec = showsPrec

deriving instance (Show (v f), Show (f (First (Maybe e)))) => Show (ErrorV e v f)

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

deriving instance (Semigroup (g (First (Maybe err))), Semigroup (v g), View v) => Commutative (ErrorV err v g)
deriving instance
  ( Semigroup (g (First (Maybe err)))
  , Group (g (First (Maybe err)))
  , Semigroup (v g)
  , Group (v g)
  , View v
  ) => Group (ErrorV err v g)
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
  , QueryResult (v (Const g)) ~ v Identity
  ) => Query (ErrorV err v (Const g)) where
  type QueryResult (ErrorV err v (Const g)) = ErrorV err v Identity
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

instance
  ( View v
  , Has' Semigroup (ErrorVK err v) (FlipAp (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g)))))
  , Query (Vessel (ErrorVK err v) g)
  ) => Query (ErrorV err v (Compose c g)) where
  type QueryResult (ErrorV err v (Compose c g)) = ErrorV err v (Compose c (VesselLeafWrapper (QueryResult (Vessel (ErrorVK err v) g))))
  crop (ErrorV s) (ErrorV r) = ErrorV $ crop s r

-- | Construct a query that registers interest in both the success and error parts of an ErrorV.
queryErrorV :: (View v, Semigroup (v Proxy)) => v Proxy -> ErrorV e v Proxy
queryErrorV v = ErrorV (singletonV ErrorVK_View v <> singletonV ErrorVK_Error (SingleV Proxy))

-- | Construct a query that registers interest in both the success and error parts of an ErrorV.
queryErrorVConst :: (View v, Num x, Semigroup x, Semigroup (v (Const x))) => v (Const x) -> ErrorV e v (Const x)
queryErrorVConst v = ErrorV (singletonV ErrorVK_View v <> singletonV ErrorVK_Error (SingleV (Const 1)))

-- | The error part of the view will never be present
successErrorV :: View v => v Identity -> ErrorV e v Identity
successErrorV = ErrorV . singletonV ErrorVK_View

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
    Right val -> pure $ successErrorV val

-- | Given an 'ErrorV' result, observe whether it is an error result
-- or a result of the underlying view type.
observeErrorV
  :: ErrorV e v Identity
  -> Maybe (Either e (v Identity))
observeErrorV (ErrorV v) = case lookupV ErrorVK_Error v of
  Nothing -> Right <$> lookupV ErrorVK_View v
  Just err -> case lookupSingleV err of
    Nothing -> Right <$> lookupV ErrorVK_View v
    Just e -> Just (Left e)

-- | A 'Path' which abstracts over constructing the query and observing the result.
errorV :: (Semigroup (v (Const x)), View v, Num x, Semigroup x)
       => Path (v (Const x)) (ErrorV e v (Const x)) (ErrorV e v Identity) (Either e (v Identity))
errorV = Path { _path_to = queryErrorVConst, _path_from = observeErrorV }

-- | Given an 'ErrorV' result, observe both error and result
-- of the underlying view type.
unsafeObserveErrorV
  :: ErrorV e v f
  -> (Maybe (f (First (Maybe e))), Maybe (v f))
unsafeObserveErrorV (ErrorV v) =
  let
    err = fmap unSingleV $ lookupV ErrorVK_Error v
  in (err, lookupV ErrorVK_View v)
