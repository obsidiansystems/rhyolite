{-|
Description:
  'Query' helpers

Miscellaneous utilities related to the 'Query' datatype, and the definition of
'Single', a view for a single piece of data.
-}
{-# Language DeriveFoldable #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveTraversable #-}
{-# Language FlexibleContexts #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language PatternGuards #-}
{-# Language PolyKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeApplications #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.App where

import Data.Aeson (FromJSON, ToJSON)
import Data.Align
import qualified Data.AppendMap as MonoidalMap
import Data.Constraint.Extras
import qualified Data.Dependent.Map as DMap'
import qualified Data.Dependent.Map.Monoidal as DMap
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MMap
import Data.Proxy
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Semigroup (First(..))
import Data.These
import Data.These.Combinators
import Data.Typeable (Typeable)
import Data.Vessel
import Data.Vessel.Internal (VSum(..))
import Data.Witherable (Filterable(..))
import GHC.Generics (Generic)
import Reflex.Query.Class

-- | Set-subtraction operation for queries.
class PositivePart q where
  positivePart :: q -> Maybe q
  -- ^ Filter a query to only those parts which are selected a positive
  -- amount.

instance PositivePart SelectedCount where
  positivePart x
    | x > 0 = Just x
    | otherwise = Nothing

instance PositivePart () where
  positivePart _ = Nothing

instance PositivePart a => PositivePart (Identity a) where
  positivePart (Identity x) = Identity <$> positivePart x

instance PositivePart x => PositivePart (Const x a) where
  positivePart (Const x) = Const <$> positivePart x

instance PositivePart (Proxy a) where
  positivePart _ = Nothing

instance (PositivePart a, PositivePart b) => PositivePart (These a b) where
  positivePart ab = align (positivePart =<< justHere ab) (positivePart =<< justThere ab)

instance PositivePart (f (g x)) => PositivePart (Compose f g x) where
  positivePart (Compose xs) = Compose <$> positivePart xs

instance Has' PositivePart f g => PositivePart (DSum f g) where
  positivePart (f :=> g) = (f :=>) <$> has' @PositivePart @g f (positivePart g)

instance (GCompare f, Has' PositivePart f g) => PositivePart (DMap'.DMap f g) where
  positivePart xs
      | DMap'.null xs' = Nothing
      | otherwise = Just xs'
    where xs' = DMap'.mapMaybeWithKey (\f g -> has' @PositivePart @g f (positivePart g)) xs

deriving instance (GCompare f, Has' PositivePart f g) => PositivePart (DMap.MonoidalDMap f g)

instance HasV PositivePart f g => PositivePart (VSum f g) where
  positivePart (f :~> g) = (f :~>) <$> hasV @PositivePart @g f (positivePart g)

deriving instance PositivePart (g (First (Maybe v))) => PositivePart (SingleV v g)

deriving instance PositivePart (g v) => PositivePart (MapV k v g)
deriving instance PositivePart (g v) => PositivePart (IdentityV v g)
deriving instance PositivePart (g f) => PositivePart (FlipAp f g)
deriving instance (GCompare f, Has' PositivePart f (FlipAp g)) => PositivePart (Vessel f g)
deriving instance (Ord k, PositivePart (f g)) => PositivePart (SubVessel k f g)

instance PositivePart a => PositivePart [a] where positivePart = composePositivePart
instance PositivePart a => PositivePart (Maybe a) where positivePart = composePositivePart
instance PositivePart a => PositivePart (Map k a) where positivePart = composePositivePart
deriving instance PositivePart a => PositivePart (MMap.MonoidalMap k a)

-- | This can be used to implement an instance of PositivePart for
-- Functor-style queries/views, in terms of the other instances already
-- required for those.
standardPositivePart :: (Eq (q a), Monoid (q a), Num a, Ord a, Filterable q) => q a -> Maybe (q a)
standardPositivePart x =
  let u = mapMaybe (\n -> if n > 0 then Just n else Nothing) x
  in if u == mempty then Nothing else Just u

-- | Map 'positivePart' over a structure. If the resulting structure is empty,
-- 'Nothing' is returned
composePositivePart :: (Foldable q, PositivePart a, Filterable q) => q a -> Maybe (q a)
composePositivePart x =
  let u = mapMaybe positivePart x
  in if null u then Nothing else Just u
{-# INLINE composePositivePart #-}

-- | Converts between a query and a map of queries with at most one element
-- (at the provided key)
singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (MonoidalMap k q)
singletonQuery k = QueryMorphism
  { _queryMorphism_mapQuery = MonoidalMap.singleton k
  , _queryMorphism_mapQueryResult = MonoidalMap.findWithDefault mempty k
  }

{-# Deprecated cropView "Use 'crop' instead" #-}
-- | See 'crop'
cropView :: (Query q) => q -> QueryResult q -> QueryResult q
cropView = crop

-- | Specialized 'mapMaybe' for working with pairs where we only want to filter
-- based on some property of the first element
fmapMaybeFst :: Filterable f => (a -> Maybe b) -> f (a, c) -> f (b, c)
fmapMaybeFst f = mapMaybe $ \(a, c) -> case f a of
  Nothing -> Nothing
  Just b -> Just (b, c)

-- | A view for a single piece of data, supporting update and delete.
newtype Single t a = Single { unSingle :: Maybe (Semigroup.First (Maybe t), a) }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic, Typeable)

instance Semigroup a => Semigroup (Single t a) where
  (<>) (Single Nothing) y = y
  (<>) x (Single Nothing) = x
  (<>) (Single (Just (t, a))) (Single (Just (_t, a'))) = Single $ Just (t, a Semigroup.<> a')

instance Semigroup a => Monoid (Single t a) where
  mempty = Single Nothing
  mappend = (Semigroup.<>)

instance Filterable (Single t) where
  mapMaybe f (Single (Just (t, x))) | Just y <- f x = Single (Just (t, y))
  mapMaybe _ _ = Single Nothing

instance (FromJSON t, FromJSON a) => FromJSON (Single t a)
instance (ToJSON t, ToJSON a) => ToJSON (Single t a)

-- | Get the data out of a 'Single'
getSingle :: Single t a -> Maybe t
getSingle (Single (Just (Semigroup.First (Just t), _))) = Just t
getSingle _ = Nothing

-- | Wrap data in a 'Single'
single :: Maybe t -> a -> Single t a
single t a = Single $ Just (Semigroup.First t, a)
