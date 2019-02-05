{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Rhyolite.Frontend.WebSocket where

import Data.Text (Text)
import Language.Javascript.JSaddle.Types
import Reflex.Dom.Core
import qualified Reflex.Dom.WebSocket as RDWS

-- | Warning: Only one of these websockets may be opened on a given page in most browsers
{-# DEPRECATED webSocket "Use Reflex.Dom.WebSocket.webSocket instead" #-}
webSocket
  :: forall x t m. (HasJS x m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadJSM m, MonadJSM (Performable m), HasJSContext m)
  => Text
  -> WebSocketConfig t Text
  -> m (WebSocket t)
webSocket = RDWS.webSocket
