-- | This module contains a newtype wrapper around 'Data.Map.Map' that has a
-- correct 'Group' instance compared to the one for
-- 'Data.Map.Monoidal.MonoidalMap', in that it has a unique neutral element.
-- This comes with different constraints on the parameters (check the instances
-- for 'Semigroup' and 'Monoid' of the corresponding data structures if you're
-- interested).
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-} -- For (Eq (QueryResult q), Ord k, Query q) => Query (MonoidMap k q)
module Data.MonoidMap where

import Data.Witherable
import Data.AppendMap
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal as Map
import Data.Semigroup (Semigroup, (<>))
import Reflex (Query, QueryResult, crop, Group(..), Additive)

newtype MonoidMap k v = MonoidMap { unMonoidMap :: MonoidalMap k v }
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable, Filterable)

monoidMap :: (Ord k, Eq v, Monoid v) => MonoidalMap k v -> MonoidMap k v
monoidMap = MonoidMap . Map.filter (/= mempty)

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
