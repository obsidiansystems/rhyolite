{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.App where

import Control.Category (Category)
import qualified Control.Category as Cat
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Align)
import Data.Semigroup (Semigroup)
import Data.Functor.Identity (Identity)

import Data.AppendMap (AppendMap)
import qualified Data.AppendMap as AppendMap
import Reflex.FunctorMaybe (FunctorMaybe, fmapMaybe)
import Reflex.Query.Base (mapQuery, mapQueryResult)
import Reflex.Query.Class (Query, QueryMorphism(..), QueryResult, SelectedCount, crop)
import Reflex.Patch (Group, Additive)

import Rhyolite.Account (AuthToken)
import Rhyolite.Request.Class (Request)
import Rhyolite.Sign (Signed)

instance (Ord k, Query v) => Query (AppendMap k v) where
  type QueryResult (AppendMap k v) = AppendMap k (QueryResult v)
  crop q r = AppendMap.intersectionWith (flip crop) r q

singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (AppendMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = AppendMap.singleton k
                                 , _queryMorphism_mapQueryResult = AppendMap.findWithDefault mempty k
                                 }

instance Category QueryMorphism where
  id = QueryMorphism id id
  qm . qm' = QueryMorphism
    { _queryMorphism_mapQuery = mapQuery qm . mapQuery qm'
    , _queryMorphism_mapQueryResult = mapQueryResult qm' . mapQueryResult qm
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
