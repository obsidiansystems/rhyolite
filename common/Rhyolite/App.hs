{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.App where

import Control.Category (Category)
import qualified Control.Category as Cat
import Data.Aeson (FromJSON, ToJSON)
import Data.Constraint.Extras
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Some as Some
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import Reflex.FunctorMaybe (FunctorMaybe, fmapMaybe)
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryMorphism(..), QueryResult, crop)

type Request r = (FromJSON (Some r), ToJSON (Some r), Has FromJSON r, Has ToJSON r)

instance (Ord k, Query v) => Query (MonoidalMap k v) where
  type QueryResult (MonoidalMap k v) = MonoidalMap k (QueryResult v)
  crop q r = MonoidalMap.intersectionWith (flip crop) r q

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (MonoidalMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = MonoidalMap.singleton k
                                 , _queryMorphism_mapQueryResult = MonoidalMap.findWithDefault mempty k
                                 }

instance Category QueryMorphism where
  id = QueryMorphism id id
  qm . qm' = QueryMorphism
    { _queryMorphism_mapQuery = mapQuery qm . mapQuery qm'
    , _queryMorphism_mapQueryResult = mapQueryResult qm' . mapQueryResult qm
    }

fmapMaybeFst :: FunctorMaybe f => (a -> Maybe b) -> f (a, c) -> f (b, c)
fmapMaybeFst f = fmapMaybe $ \(a, c) -> case f a of
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

instance FunctorMaybe (Single t) where
  fmapMaybe f (Single (Just (t, x))) | Just y <- f x = Single (Just (t, y))
  fmapMaybe _ _ = Single Nothing

instance (FromJSON t, FromJSON a) => FromJSON (Single t a)
instance (ToJSON t, ToJSON a) => ToJSON (Single t a)

getSingle :: Single t a -> Maybe t
getSingle (Single (Just (Semigroup.First (Just t), _))) = Just t
getSingle _ = Nothing

single :: Maybe t -> a -> Single t a
single t a = Single $ Just (Semigroup.First t, a)
