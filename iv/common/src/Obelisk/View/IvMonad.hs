module Obelisk.View.IvMonad where

import Control.Monad.Ref.Restricted
import Control.Monad.MVar.Restricted
import Control.Monad.Fix

-- | A monad that can be used as the execution environment for an Iv
type IvMonad m =
  ( MonadAtomicRef m
  , MonadMVar m
  , MonadFix m
  )

