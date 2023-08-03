{-| Description: Class for modal creators
A class for widget that can have modals. A concrete implementation is in
"Reflex.Dom.Modal.Base".
-}

{-# Language DefaultSignatures #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeFamilies #-}

module Reflex.Dom.Modal.Class where

import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.Trans (MonadTrans (lift))
import Obelisk.Route.Frontend (RoutedT, askRoute, runRoutedT)
import Reflex (Event, EventWriterT, Reflex)

-- | The class of monads supporting a 'tellModal' operation which will open a modal
--   that stays on top of all other content.
class HasModal t m where
  -- If 'm' is the monad that supports 'tellModal' then 'ModalM m' is the monad that the modal itself is in,
  -- which, notably, probably doesn't support 'tellModal'.
  type ModalM m :: * -> *

  -- | Opens a modal when the given event fires. The event carries a function which:
  --     * takes a "close" event triggered when the user signifies that they want to close the modal,
  --     * builds content in 'ModalM m',
  --     * returns a "close" event which will be used to actually close the modal.
  --
  --   For example, a modal may choose not to be closable by simply ignoring its input and returning 'never'.
  --
  --   Note on positioning: We control the containing div of your dialog for
  --   handling of the CSS display property. We recommend that you position
  --   your dialog `fixed` with some Clay like the following:
  --
  -- @
  --  position fixed
  --  top (pct 50)
  --  left (pct 50)
  --  transform (translate (pct $ negate 50) (pct $ negate 50))
  -- @
  tellModal :: Event t (Event t () -> ModalM m (Event t ())) -> m ()

  default tellModal :: (MonadTrans f, m ~ f m', HasModal t m', Monad m', ModalM (f m') ~ ModalM m') => Event t (Event t () -> ModalM m (Event t ())) -> m ()
  tellModal = lift . tellModal

instance (Monad m, Reflex t, HasModal t m) => HasModal t (EventWriterT t w m) where
  type ModalM (EventWriterT t w m) = ModalM m

instance (Monad m, Reflex t, HasModal t m) => HasModal t (ReaderT r m) where
  type ModalM (ReaderT r m) = ReaderT r (ModalM m) -- Transform the modal's monad
  tellModal ev = do
    r <- ask
    lift $ tellModal $ (fmap . fmap) (`runReaderT` r) ev

instance (Monad m, Reflex t, HasModal t m) => HasModal t (RoutedT t r m) where
  type ModalM (RoutedT t r m) = RoutedT t r (ModalM m) -- Transform the modal's monad
  tellModal ev = do
    r <- askRoute
    lift $ tellModal $ (fmap . fmap) (`runRoutedT` r) ev
