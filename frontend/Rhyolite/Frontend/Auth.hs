{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Rhyolite.Frontend.Auth where

import Control.Category
import Control.Monad.Fix
import Data.Functor.Const
import Prelude hiding ((.), id)
import Reflex

import Rhyolite.Api
import Rhyolite.Frontend.App
import Rhyolite.Vessel.AuthMapV
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.Types

-- | The type for app widgets at the top level of the app.
-- Includes full Auth handling for queries and requests.
type FullWidget token publicApi privateApi publicV privateV personalV =
  RhyoliteWidget
    (FullV token publicV privateV personalV (Const SelectedCount))
    (ApiRequest token publicApi privateApi)

type FullWidgetInternal token publicApi privateApi publicV privateV personalV t m =
  RhyoliteWidgetInternal
    (FullV token publicV privateV personalV (Const SelectedCount))
    (ApiRequest token publicApi privateApi)
    t m

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity that may or may not be valid, and so the queries
-- can fail.
type AuthErrorWidget token publicApi privateApi publicV privateV personalV =
  RhyoliteWidget
    (FullAuthErrorV publicV privateV personalV (Const SelectedCount))
    (ApiRequest () publicApi privateApi)

type AuthErrorWidgetInternal token publicApi privateApi publicV privateV personalV t m =
  RhyoliteWidgetInternal
    (FullAuthErrorV publicV privateV personalV (Const SelectedCount))
    (ApiRequest () publicApi privateApi)
    t m

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity where authentication errors have already been handled
-- so the queries cannot fail within this widget.
type AuthWidget token publicApi privateApi publicV privateV personalV =
  RhyoliteWidget
    (AuthenticatedV publicV privateV personalV (Const SelectedCount))
    (ApiRequest () publicApi privateApi )

type AuthWidgetInternal token publicApi privateApi publicV privateV personalV t m =
  RhyoliteWidgetInternal
    (AuthenticatedV publicV privateV personalV (Const SelectedCount))
    (ApiRequest () publicApi privateApi )
    t m

-- | Embeds a widget that uses a specific auth identity into a context where no auth identity is presumed.
authenticatedWidget
  :: ( MonadFix m, PostBuild t m
     , HasRhyoliteAuth token publicV privateV personalV )
  => token
  -> AuthErrorWidget token publicApi privateApi publicV privateV personalV t m a
  -> FullWidget token publicApi privateApi publicV privateV personalV t m a
authenticatedWidget token = mapAuth token (mapAuthenticatedV id (authMapQueryMorphism token) (authMapQueryMorphism token)) . unRhyoliteWidget
