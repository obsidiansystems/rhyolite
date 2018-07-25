{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Email where

import Control.Monad.Reader (ReaderT, lift)
import Network.Mail.Mime (Mail)

class Monad m => MonadEmail m where
  sendMail :: Mail -> m ()

instance MonadEmail m => MonadEmail (ReaderT r m) where
  sendMail = lift . sendMail
