-- | Miscellaneous utilities related to the 'Query' datatype, and the definition
-- of 'Single', a view for a single piece of data.
--
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
class PositivePart q where
  positivePart :: q -> Maybe q -- ^ Filter a query to only those parts which are selected a positive amount.

-- | This can be used to implement an instance of PositivePart for Functor-style queries/views, in terms of the other instances already required for those.
standardPositivePart :: (Eq (q a), Monoid (q a), Num a, Ord a, Filterable q) => q a -> Maybe (q a)
standardPositivePart x =
  let u = mapMaybe (\n -> if n > 0 then Just n else Nothing) x
  in if u == mempty then Nothing else Just u

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
