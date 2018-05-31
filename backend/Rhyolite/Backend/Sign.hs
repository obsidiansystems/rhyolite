{-# LANGUAGE ScopedTypeVariables #-}
--{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.Backend.Sign where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Typeable, typeOf)
import qualified Web.ClientSession as CS

import Rhyolite.Request.Common (decodeValue')
import Rhyolite.Sign (Signed (..))

signWithKey :: (Typeable b, ToJSON b, MonadIO m) => CS.Key -> b -> m (Signed a)
signWithKey k (v :: b) =
  liftIO $ fmap (Signed . decodeUtf8) $ CS.encryptIO k $ LBS.toStrict $ encode (show $ typeOf (undefined :: b), v)

readSignedWithKey :: (Typeable a, FromJSON a) => CS.Key -> Signed a -> Maybe a
readSignedWithKey k s = do
    tvJson <- CS.decrypt k $ encodeUtf8 $ unSigned s
    (t, v :: b) <- decodeValue' $ LBS.fromStrict tvJson
    guard $ t == show (typeOf (undefined :: b))
    return v
