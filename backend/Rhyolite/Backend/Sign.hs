-- | Functions to encrypt the cookie data using "Web.ClientSession", see the
-- relevant datatypes in "Rhyolite.Sign".

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

module Rhyolite.Backend.Sign where

import Control.Monad (guard)
import Control.Monad.Base
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable, typeRep)
import Database.Groundhog (DbPersist)
import qualified Web.ClientSession as CS

import Rhyolite.Backend.Schema.TH (deriveNewtypePersistBackend)
import Rhyolite.Backend.DB.LargeObjects (PostgresLargeObject (withLargeObject))
import Rhyolite.Backend.DB.PsqlSimple (PostgresRaw)
import Rhyolite.Email (MonadEmail)
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route (MonadRoute)
import Rhyolite.Sign (MonadSign (..), Signed (..))

signWithKey :: (Typeable b, ToJSON b, MonadIO m) => CS.Key -> b -> m (Signed a)
signWithKey k (v :: b) =
  liftIO $ fmap (Signed . decodeUtf8) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeRep (Proxy @b), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
  tvJson <- CS.decrypt k $ encodeUtf8 $ unSigned s
  (t, v :: b) <- decodeValue' $ LBS.fromStrict tvJson
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
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadEmail, MonadRoute r, MonadLogger)

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

deriveNewtypePersistBackend (\m -> [t| SignT $m |]) (\m -> [t| ReaderT CS.Key $m |]) 'SignT 'unSignT

instance (Monad m, PostgresRaw m) => PostgresRaw (SignT m)
instance (Monad m, PostgresLargeObject m) => PostgresLargeObject (SignT m) where
  withLargeObject oid mode f = do
    k <- SignT ask
    lift $ withLargeObject oid mode (\lofd -> runSignT (f lofd) k)

-- Orphans
instance MonadSign m => MonadSign (NoLoggingT m) where
  type SigningKey (NoLoggingT m) = SigningKey m

instance MonadSign m => MonadSign (DbPersist conn m) where
  type SigningKey (DbPersist conn m) = SigningKey m
