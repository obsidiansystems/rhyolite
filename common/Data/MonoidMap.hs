{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- For (Eq (QueryResult q), Ord k, Query q) => Query (MonoidMap k q)
module Data.MonoidMap
  ( MonoidMap (unMonoidMap)
  , Data.MonoidMap.lookup
  , mapWithKey
  , monoidMap
  , toAscList
  , unsafeMkMonoidMap
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Semialign, Align)
import Data.AppendMap ()
import Data.Coerce (coerce)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as Map
import Data.Semigroup (Semigroup, (<>))
import Data.Witherable (Filterable)
import Reflex (Query, QueryResult, crop, Group(..), Additive)

newtype MonoidMap k v = MonoidMap { unMonoidMap :: MonoidalMap k v }
  deriving stock (Show, Eq, Ord, Foldable, Functor, Traversable)
  deriving newtype (Filterable, Align, Semialign, FromJSON, ToJSON)

monoidMap :: (Ord k, Eq v, Monoid v) => MonoidalMap k v -> MonoidMap k v
monoidMap = MonoidMap . Map.filter (/= mempty)

-- | Construct a 'MonoidMap' from a 'MonoidalMap' with the *assumption* that it contains no 'mempty's. Condition is not checked!
-- For the safe constructor, use 'monoidMap'.
unsafeMkMonoidMap :: MonoidalMap k v -> MonoidMap k v
unsafeMkMonoidMap = MonoidMap

instance (Eq (QueryResult q), Ord k, Query q) => Query (MonoidMap k q) where
  type QueryResult (MonoidMap k q) = MonoidMap k (QueryResult q)
  crop (MonoidMap q) (MonoidMap qr) =
    -- This assumes that the query result of a null query should be null
    monoidMap $ Map.intersectionWith crop q qr

instance (Monoid a, Eq a, Ord k) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b =
    let combine _ a' b' =
          let c = a' `mappend` b'
          in if c == mempty
               then Nothing
               else Just c
    in MonoidMap $ Map.mergeWithKey combine id id a b

instance (Ord k, Monoid a, Eq a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap Map.empty
  mappend = (<>)

instance (Ord k, Monoid a, Eq a, Group a) => Group (MonoidMap k a) where
  negateG = fmap negateG

instance (Ord k, Monoid a, Eq a, Group a, Additive a) => Additive (MonoidMap k a)

singleton :: Monoid a => k -> a -> MonoidMap k a
singleton _ v | v == mempty = mempty
singleton k v = MonoidMap $ Map.singleton k v

toAscList :: forall k a. MonoidMap k a -> [(k, a)]
toAscList = coerce (Map.toAscList :: MonoidalMap k a -> [(k, a)])

lookup :: forall k a. Ord k => k -> MonoidMap k a -> Maybe a
lookup = coerce (Map.lookup :: k -> MonoidalMap k a -> Maybe a)

mapWithKey :: forall k a b. (Eq b, Monoid b) => (k -> a -> b) -> MonoidMap k a -> MonoidMap k b
mapWithKey f m = MonoidMap $ Map.mapMaybeWithKey (\k a -> let r = f k a in if r == mempty then Nothing else Just r) (coerce m)
