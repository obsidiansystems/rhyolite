module Rhyolite.Frontend.Auth.App
  ( module Rhyolite.Frontend.Auth.App
  , module X
  ) where

import qualified Rhyolite.Frontend.Auth as Base
import Rhyolite.Vessel.App as X
import Control.Monad.Fix
import Reflex


-- | The type for app widgets at the top level of the app.
-- Includes full Auth handling for queries and requests.
type FullAppWidget t app m =
  Base.FullWidget t (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app) m

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity that may or may not be valid, and so the queries
-- can fail.
type AuthErrorAppWidget t app m =
  Base.AuthErrorWidget t (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app) m

-- | The type for app widgets that have been specialized to a particular
-- authenticated identity where authentication errors have already been handled
-- so the queries cannot fail within this widget.
type AuthAppWidget t app m =
  Base.AuthWidget t (AuthCredential app) (PublicApi app) (PrivateApi app) (PublicV app) (PrivateV app) (PersonalV app) m

-- | Embeds a widget that uses a specific auth identity into a context where no auth identity is presumed.
authenticatedWidget
  :: ( MonadFix m, PostBuild t m
     , RhyoliteAuthApp app )
  => proxy app
  -> AuthCredential app
  -> AuthErrorAppWidget t app m a
  -> FullAppWidget t app m a
authenticatedWidget _ = Base.authenticatedWidget


