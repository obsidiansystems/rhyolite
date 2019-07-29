{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Rhyolite.Frontend.Modal.Class where

import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.Trans (MonadTrans (lift))
import Reflex (Event, Reflex, EventWriterT)
import Obelisk.Route.Frontend (RoutedT, askRoute, runRoutedT)

-- | The class of monads supporting a 'tellModal' operation which will open a modal
--   that stays on top of all other content.
class HasModal t m where
  type ModalM m :: * -> *
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
