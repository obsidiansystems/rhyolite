module Rhyolite.Frontend.Auth.App
  ( module Rhyolite.Frontend.Auth.App
  , module X
  ) where

import qualified Rhyolite.Frontend.Auth as Base
import Rhyolite.Vessel.App as X
import Control.Monad.Fix
import Reflex
import Data.Functor.Const
import Data.Functor.Identity


-- | Embeds a widget that uses a specific auth identity into a context where no auth identity is presumed.
authenticatedWidget
  :: ( MonadFix m, PostBuild t m
     , RhyoliteAuthApp app )
  => proxy app
  -> (AuthCredential app)
  -> QueryT t (FullAppAuthErrorV app (Const SelectedCount)) (RequesterT t (FullAuthApi app) Identity m) a
  -> QueryT t (FullAppV app (Const SelectedCount)) (RequesterT t (FullApi app) Identity m) a
authenticatedWidget _ = Base.authenticatedWidget
