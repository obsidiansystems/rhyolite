-- | Monad and instances to send a mail.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Email where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Except (ExceptT)
import Network.Mail.Mime (Mail)

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail

instance MonadEmail m => MonadEmail (MaybeT m) where
  sendMail = lift . sendMail
instance MonadEmail m => MonadEmail (ExceptT e m) where
  sendMail = lift . sendMail
