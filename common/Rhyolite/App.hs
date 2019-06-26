{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.App where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.AppendMap as MonoidalMap
import Data.Map.Monoidal (MonoidalMap)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Typeable (Typeable)
import Data.Witherable (Filterable(..))
import GHC.Generics (Generic)
import Reflex.Query.Class

-- | Set-subtraction operation for queries.
class (Query q, Eq q) => DiffQuery q where
  diffQuery :: q -> q -> Maybe q -- ^ diffQuery x y indicates interest in the part of x which is not indicated by y. Results in Nothing if this difference is empty.

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (MonoidalMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = MonoidalMap.singleton k
                                 , _queryMorphism_mapQueryResult = MonoidalMap.findWithDefault mempty k
                                 }

cropView :: (Query q) => q -> QueryResult q -> QueryResult q
cropView = crop

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

getSingle :: Single t a -> Maybe t
getSingle (Single (Just (Semigroup.First (Just t), _))) = Just t
getSingle _ = Nothing

single :: Maybe t -> a -> Single t a
single t a = Single $ Just (Semigroup.First t, a)
