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

import Control.Category (Category)
import qualified Control.Category as Cat
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Align)
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as Semigroup
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import Reflex.FunctorMaybe (FunctorMaybe, fmapMaybe)
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryMorphism(..), QueryResult, SelectedCount, crop)
import Reflex.Patch (Group, Additive)
import qualified Data.AppendMap as MonoidalMap

import Rhyolite.Account (AuthToken)
import Rhyolite.Request.Class (Request)
import Rhyolite.Sign (Signed)

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (MonoidalMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = MonoidalMap.singleton k
                                 , _queryMorphism_mapQueryResult = MonoidalMap.findWithDefault mempty k
                                 }

class ( ToJSON (ViewSelector app ()), FromJSON (ViewSelector app ())
      , ToJSON (View app ()), FromJSON (View app ())
      , Monoid (ViewSelector app SelectedCount), Semigroup (ViewSelector app SelectedCount)
      , Group (ViewSelector app SelectedCount), Additive (ViewSelector app SelectedCount)
      , Query (ViewSelector app SelectedCount), QueryResult (ViewSelector app SelectedCount) ~ View app SelectedCount
      , Align (ViewSelector app), FunctorMaybe (ViewSelector app), Foldable (ViewSelector app)
      , Traversable (ViewSelector app)
      , Eq (ViewSelector app SelectedCount)
      , Eq (View app ()), Show (View app ()), Functor (View app), Eq (View app SelectedCount)
      ) => HasView app where
  type View app :: * -> *
  type ViewSelector app :: * -> *

cropView :: (Query q) => q -> QueryResult q -> QueryResult q
cropView = crop

class (Request (PublicRequest app), Request (PrivateRequest app), ToJSON (AppCredential app), FromJSON (AppCredential app), Eq (AppCredential app)) => HasRequest app where
  data PublicRequest app :: * -> *
  data PrivateRequest app :: * -> *
  type AppCredential app :: *
  type AppCredential app = Signed (AuthToken Identity)

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
