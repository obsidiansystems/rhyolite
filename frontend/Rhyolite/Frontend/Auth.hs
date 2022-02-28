{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Rhyolite.Frontend.Auth where

import Control.Category
import Control.Monad.Fix
import Data.Functor.Const
import Data.Functor.Identity
import Data.Semigroup
import Prelude hiding ((.), id)
import Reflex

import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.Types

-- | The type for app widgets at the top level of the app.
-- Includes full Auth handling for queries and requests.
type FullWidget t token publicApi privateApi publicV privateV personalV m =
    (EventWriterT t (First (Maybe token))
      (QueryT t (FullV token publicV privateV personalV (Const SelectedCount))
        (RequesterT t (ApiRequest token publicApi privateApi) Identity m)))

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity that may or may not be valid, and so the queries
-- can fail.
type AuthErrorAppWidget t token publicApi privateApi publicV privateV personalV m =
  (EventWriterT t (First (Maybe token))
      (QueryT t (FullAuthErrorV publicV privateV personalV (Const SelectedCount))
        (RequesterT t (ApiRequest () publicApi privateApi) Identity m)))

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity where authentication errors have already been handled
-- so the queries cannot fail within this widget.
type AuthAppWidget t token publicApi privateApi publicV privateV personalV m =
    (EventWriterT t (First (Maybe token))
      (QueryT t (AuthenticatedV publicV privateV personalV (Const SelectedCount))
        (RequesterT t (ApiRequest () publicApi privateApi ) Identity m)))

-- | Embeds a widget that uses a specific auth identity into a context where no auth identity is presumed.
authenticatedWidget
  :: ( MonadFix m, PostBuild t m
     , HasRhyoliteAuth token publicV privateV personalV )
  => token
  -> QueryT t (FullAuthErrorV publicV privateV personalV (Const SelectedCount)) (RequesterT t (ApiRequest () publicApi privateApi) Identity m) a
  -> QueryT t (FullV token publicV privateV personalV (Const SelectedCount)) (RequesterT t (ApiRequest token publicApi privateApi) Identity m) a
authenticatedWidget token = unRhyoliteWidget . mapAuth token (mapAuthenticatedV id (authMapQueryMorphism token) (authMapQueryMorphism token))
