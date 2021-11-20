-- | Functions to encrypt data using "Web.ClientSession"


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Signed.ClientSession
  ( module Data.Signed.ClientSession
  , CS.Key
  , CS.getKey
  ) where

import Control.Monad (guard)
import Control.Monad.Base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON, encode, decodeStrict')
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy(..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable, typeRep)
import qualified Web.ClientSession as CS
import Data.Signed (MonadSign(..), Signed(..))

signWithKey :: (Typeable a, ToJSON a, MonadIO m) => CS.Key -> a -> m (Signed a)
signWithKey k (v :: a) =
  liftIO $ fmap (Signed . decodeUtf8) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeRep (Proxy @a), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
  tvJson <- CS.decrypt k $ encodeUtf8 $ unSigned s
  (t, v :: b) <- decodeStrict' tvJson
  guard $ t == show (typeRep $ Proxy @b)
  return v

-- We need the Typeable here because otherwise two 'Signed's whose contents encode the same way will be interchangeable
sign :: (MonadSign m, SigningKey m ~ CS.Key, MonadIO m, Typeable a, ToJSON a) => a -> m (Signed a)
sign a = do
  k <- askSigningKey
  signWithKey k a

readSigned :: (MonadSign m, SigningKey m ~ CS.Key, Typeable a, FromJSON a) => Signed a -> m (Maybe a)
readSigned s = do
  k <- askSigningKey
  pure $ readSignedWithKey k s

newtype SignT m a = SignT { unSignT :: ReaderT CS.Key m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadLogger)

runSignT :: SignT m a -> CS.Key -> m a
runSignT (SignT a) = runReaderT a

instance MonadBase b m => MonadBase b (SignT m) where
  liftBase = SignT . liftBase

instance MonadTransControl SignT where
  type StT SignT a = StT (ReaderT CS.Key) a
  liftWith f = SignT $ liftWith $ \g -> f $ g . unSignT
  restoreT a = SignT $ restoreT a

instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (SignT m) where
  type StM (SignT m) a = StM (ReaderT CS.Key m) a
  liftBaseWith f = SignT $ liftBaseWith $ \g -> f $ g . unSignT
  restoreM a = SignT $ restoreM a

instance Monad m => MonadSign (SignT m) where
  type SigningKey (SignT m) = CS.Key
  askSigningKey = SignT ask

-- Orphans
instance MonadSign m => MonadSign (NoLoggingT m) where
  type SigningKey (NoLoggingT m) = SigningKey m
