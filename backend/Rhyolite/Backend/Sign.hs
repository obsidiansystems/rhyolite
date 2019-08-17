{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable, typeOf)
import Database.Groundhog (DbPersist)
import qualified Web.ClientSession as CS

import Rhyolite.Backend.Schema.TH (deriveNewtypePersistBackend)
import Rhyolite.Email (MonadEmail)
import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Route (MonadRoute)
import Rhyolite.Sign (MonadSign (..), Signed (..))

signWithKey :: (Typeable b, ToJSON b, MonadIO m) => CS.Key -> b -> m (Signed a)
signWithKey k (v :: b) =
  liftIO $ fmap (Signed . decodeUtf8) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeOf (undefined :: b), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
    tvJson <- CS.decrypt k $ encodeUtf8 $ unSigned s
    (t, v :: b) <- decodeValue' $ LBS.fromStrict tvJson
    guard $ t == show (typeOf (undefined :: b))
    return v

newtype SignT m a = SignT { unSignT :: ReaderT CS.Key m a } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadEmail, MonadRoute r, MonadLogger)

runSignT :: SignT m a -> CS.Key -> m a
runSignT (SignT a) r = runReaderT a r

instance (MonadIO m, MonadBase IO m) => MonadBase IO (SignT m) where
  liftBase = liftIO

instance MonadTransControl SignT where
  type StT SignT a = StT (ReaderT CS.Key) a
  liftWith f = SignT $ liftWith $ \g -> f $ g . unSignT
  restoreT a = SignT $ restoreT a

instance (MonadIO m, MonadBaseControl IO m) => MonadBaseControl IO (SignT m) where
  type StM (SignT m) a = StM (ReaderT CS.Key m) a
  liftBaseWith f = SignT $ liftBaseWith $ \g -> f $ g . unSignT
  restoreM a = SignT $ restoreM a

instance MonadIO m => MonadSign (SignT m) where
  sign a = do
    k <- SignT ask
    signWithKey k a
  readSigned s = do
    k <- SignT ask
    return $ readSignedWithKey k s

instance MonadSign m => MonadSign (NoLoggingT m) where
  sign = lift . sign
  readSigned = lift . readSigned

instance MonadSign m => MonadSign (DbPersist conn m) where
  sign = lift . sign
  readSigned = lift . readSigned

deriveNewtypePersistBackend (\m -> [t| SignT $m |]) (\m -> [t| ReaderT CS.Key $m |]) 'SignT 'unSignT
