-- | Infrastructure for signing data, which you would use, e.g. when signing cookies.

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rhyolite.Sign where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.State.Strict as Strict

-- | A wrapper around 'Text' that contains the signed payload.
newtype Signed a = Signed { unSigned :: Text }
  deriving (Show, Read, Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class Monad m => MonadSign m where
  type SigningKey m
  askSigningKey :: m (SigningKey m)
  default askSigningKey
    :: (m ~ t m', SigningKey (t m') ~ SigningKey m', MonadTrans t, MonadSign m', Monad m')
    => m (SigningKey m)
  askSigningKey = lift askSigningKey

instance MonadSign m => MonadSign (ReaderT r m) where
  type SigningKey (ReaderT r m) = SigningKey m

instance MonadSign m => MonadSign (StateT s m) where
  type SigningKey (StateT s m) = SigningKey m

instance MonadSign m => MonadSign (Strict.StateT s m) where
  type SigningKey (Strict.StateT s m) = SigningKey m

instance MonadSign m => MonadSign (MaybeT m) where
  type SigningKey (MaybeT m) = SigningKey m

instance MonadSign m => MonadSign (ExceptT e m) where
  type SigningKey (ExceptT e m) = SigningKey m
