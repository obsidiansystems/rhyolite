{-|
Description:
  Modal widgets

The important definition here is 'ModalT', the related class is
in "Reflex.Dom.Modal.Class".
-}

{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}

module Reflex.Dom.Modal.Base where

import Control.Applicative (liftA2)
import Control.Lens (Rewrapped, Wrapped (Unwrapped, _Wrapped'), iso)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Ref (MonadAtomicRef, MonadRef)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Coerce (coerce)
import Data.Either.Combinators (rightToMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Configs (HasConfigs)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Reflex.Dom.Modal.Class (HasModal (ModalM, tellModal))

instance (Reflex t, Monad m) => HasModal t (ModalT t modalM m) where
  type ModalM (ModalT t modalM m) = modalM
  tellModal = ModalT . tellEvent . fmap First

-- | Modal monad transformer
newtype ModalT t modalM m a
  = ModalT { unModalT :: EventWriterT t (First (Event t () -> modalM (Event t ()))) m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef, MonadReader r
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , PerformEvent t, TriggerEvent t, PostBuild t
    , MonadReflexCreateTrigger t, MonadQuery t q, Requester t
    )

instance PrimMonad m => PrimMonad (ModalT t modalM m) where
  type PrimState (ModalT t modalM m) = PrimState m
  primitive = lift . primitive

instance Wrapped (ModalT t modalM m a) where
  type Unwrapped (ModalT t modalM m a) = EventWriterT t (First (Event t () -> modalM (Event t ()))) m a
  _Wrapped' = iso coerce coerce
instance ModalT t modalM m a ~ x => Rewrapped (ModalT t modalM m a) x

instance HasDocument m => HasDocument (ModalT t modalM m)
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (ModalT t modalM m)
#endif

instance (Monad m, Routed t r m) => Routed t r (ModalT t modalM m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (ModalT t modalM m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (ModalT t modalM m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (ModalT t modalM m) where
  tellEvent = lift . tellEvent

instance MonadTrans (ModalT t modalM) where
  lift = ModalT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (ModalT t modalM m) where
  runWithReplace a0 a' = ModalT $ runWithReplace (unModalT a0) (fmapCheap unModalT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = ModalT $ traverseDMapWithKeyWithAdjust (coerce f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = ModalT $ traverseDMapWithKeyWithAdjustWithMove (coerce f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = ModalT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (ModalT t modalM m)

instance (Prerender t m, Monad m, Reflex t) => Prerender t (ModalT t modalM m) where
  type Client (ModalT t modalM m) = ModalT t modalM (Client m)
  prerender back front = do
    (a, ev) <- fmap splitDynPure $ lift $ prerender
      (runEventWriterT $ unModalT back)
      (runEventWriterT $ unModalT front)
    ModalT $ tellEvent $ switchDyn ev
    pure a
    
instance HasConfigs m => HasConfigs (ModalT t modalM m)

-- | Like 'withModals' but with the full convenience of 'ModalT', allowing 'tellModal' to open a modal anywhere.
--
-- NB: This must wrap all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
runModalT
  :: forall m t a.
   ( MonadFix m
   , DomBuilder t m, MonadHold t m, PostBuild t m, Prerender t m
   )
  => ModalBackdropConfig -> ModalT t m m a -> m a
runModalT backdropCfg f = do
  rec
    ((a, open), _) <- withModals backdropCfg (getFirst <$> open) $ runEventWriterT (unModalT f)
  pure a

-- | Change the underlying monad of `ModalT` and the monad the modals will be run in.
--
--   For cases where those two monads differ, checkout `mapModalT` and `mapModalM`.
mapModalTM :: (Reflex t, MonadHold t m) => (forall x. m x -> n x) -> ModalT t m m a -> ModalT t n n a
mapModalTM f = mapModalT f . mapModalM f

-- | Change the underlying monad of `ModalT`.
mapModalT :: (Reflex t, MonadHold t m) => (forall x. m x -> n x) -> ModalT t modalM m a -> ModalT t modalM n a
mapModalT f = ModalT . mapEventWriterT f . unModalT

-- | Change the monad the modals will be run in.
mapModalM :: (Reflex t, MonadHold t m) => (forall x. modalM x -> modalN x) -> ModalT t modalM m a -> ModalT t modalN m a
mapModalM f = ModalT . withEventWriterT ((fmap . fmap) f) . unModalT

-- | You can adjust the attributes passed to the backdrop with this config.
--
--   You can adjust the background and the size, for example. You cannot change
--   the CSS `display` property, as it is controlled by this library. Also note
--   that the dialog is not a child widget of the backdrop, but rendered within
--   a separate `untouchable` top level div. For positioning of your dialog,
--   you should not rely on backdrop, nor on this hidden div, instead we
--   recommend fixed positioning for the dialog, as described in `tellModal`.
newtype ModalBackdropConfig = ModalBackdropConfig
  { _modalBackdropConfig_attrs :: Map Text Text
  } deriving (Monoid, Semigroup)

-- | Set up DOM to support modals.
--
-- NB: This must wrap all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
withModals
  :: forall m a b t.
   ( MonadFix m
   , DomBuilder t m, MonadHold t m, PostBuild t m, Prerender t m
   )
  => ModalBackdropConfig
  -> Event t (Event t () -> m (Event t a))
  -- ^ Event to trigger a modal to open.
  -- The event carries a function that takes close events and builds a modal window
  -- which returns a close event.
  -> m b -- ^ Page body
  -> m (b, Event t a) -- ^ Result of page body and an event firing whenever a modal closes
withModals backdropCfg open body = liftA2 (,) body (modalDom backdropCfg open)

-- | Builds modal-related DOM. Avoid using this and use 'withModals' instead.
--
-- NB: This must run after all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
modalDom
  :: forall a m t. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Prerender t m)
  => ModalBackdropConfig
  -> Event t (Event t () -> m (Event t a))
  -- ^ Event to trigger a modal to open.
  -- The event carries a function that takes close events and builds a modal window
  -- which returns a close event.
  -> m (Event t a) -- ^ An event firing whenever the modal closes
modalDom backdropCfg open = do
  escPressed :: Event t () <- fmap switchDyn $ prerender (pure never) $ do
    document <- DOM.currentDocumentUnchecked
    wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing
  rec
    isVisible <- holdDyn False $ leftmost [True <$ open, False <$ close]
    (backdropEl, _) <- elDynAttr' "div"
      (ffor isVisible $ \isVis ->
        ("style" =: (isVisibleStyle isVis <> ";" <> existingBackdropStyle)) <> _modalBackdropConfig_attrs backdropCfg
      )
      blank
    close <- elDynAttr "div" (ffor isVisible $ \isVis -> "style" =: isVisibleStyle isVis) $
      fmap switchDyn $ widgetHold (pure never) $ leftmost
        [ ($ leftmost [escPressed, domEvent Click backdropEl]) <$> open
        , pure never <$ close
        ]
  pure close
  where
    existingBackdropStyle = fromMaybe "" $ Map.lookup "style" $ _modalBackdropConfig_attrs backdropCfg
    isVisibleStyle isVis = "display:" <> (if isVis then "block" else "none")

-- | Widget used as a modal div for widgets that want to take some action when
-- clicked anywhere but itself, such as dropdown widgets or the like.  The
-- first argument is a CSS class name, the suggested CSS class styling for use
-- of this widget is as follows:
--
-- > position: fixed;
-- > top: 0;
-- > bottom: 0;
-- > right: 0;
-- > left: 0;
-- > z-index: 100;
--
{-# Deprecated withBackdrop "Use ModalT instead" #-}
withBackdrop :: forall m t a. (DomBuilder t m, MonadFix m, MonadHold t m) => Text -> Event t (m (Event t a)) -> m (Event t a)
withBackdrop cls openBackdropWithChild = mdo
  sth <- widgetHold (return never) $ ffor (leftmost [close, open]) $ \case
    Nothing -> return never
    Just child -> do
      (backgroundEl, _) <- elClass' "div" cls blank
      childResult <- child
      let backgroundEvent = domEvent Click backgroundEl
      return $ leftmost [Left <$> backgroundEvent, Right <$> childResult]
  let close :: Event t (Maybe (m (Event t a))) = Nothing <$ (switch . current $ sth)
      open :: Event t (Maybe (m (Event t a))) = Just <$> openBackdropWithChild
  return $ fmapMaybe rightToMaybe $ switch . current $ sth
