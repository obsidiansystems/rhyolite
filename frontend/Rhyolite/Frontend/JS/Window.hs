{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Rhyolite.Frontend.JS.Window where

import GHCJS.DOM.Types (MonadJSM)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Window as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOMEvents
import Reflex.Dom.Core
import Control.Monad
import Control.Monad.Fix

askDomWindow :: MonadJSM m => m DOM.Window
askDomWindow = DOM.currentWindowUnchecked

windowHasFocus :: ( Reflex t
                  , MonadHold t m
                  , TriggerEvent t m
                  , MonadJSM m
                  )
               => DOM.Window
               -> m (Dynamic t Bool)
windowHasFocus w = do
  f <- wrapDomEvent w (`DOM.on` DOMEvents.focus) $ return True
  b <- wrapDomEvent w (`DOM.on` DOMEvents.blur) $ return False
  holdDyn True $ leftmost [f, b] --TODO: Get the initial value properly

windowResizeEvent :: ( Reflex t
                     , TriggerEvent t m
                     , MonadJSM m )
                  => DOM.Window
                  -> m (Event t (Int, Int))
windowResizeEvent w = do
  wrapDomEvent w (`DOM.on` DOMEvents.resize) $ do
    width <- DOM.getInnerWidth w
    height <- DOM.getInnerHeight w
    return (width, height)

getWindowSize :: DOM.Window -> DOM.JSM (Int, Int)
getWindowSize w = liftM2 (,) (DOM.getInnerWidth w) (DOM.getInnerHeight w)

windowSize :: ( Reflex t
              , TriggerEvent t m
              , MonadFix m
              , PostBuild t m
              , MonadJSM (Performable m)
              , PerformEvent t m
              , MonadHold t m
              , MonadJSM m )
           => DOM.Window
           -> m (Dynamic t (Int, Int))
windowSize w = do
  initialSize <- DOM.liftJSM $ getWindowSize w
  resizeRaw <- windowResizeEvent w
  resize <- debounce 0.25 resizeRaw -- debounce because the raw resize event can be very spammy in some browsers when a user is dragging
  holdDyn initialSize resize
