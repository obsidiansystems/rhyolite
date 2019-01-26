{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rhyolite.Aeson.Orphans where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid hiding (First (..))
import Data.Semigroup
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSON LBS.ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode . LBS.toStrict

instance FromJSON LBS.ByteString where
    parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

instance ToJSONKey ByteString
instance FromJSONKey ByteString

#if !MIN_VERSION_aeson(1,0,2)
deriving instance FromJSON a => FromJSON (First a)
deriving instance ToJSON a => ToJSON (First a)
#endif

deriving instance FromJSON Any
deriving instance ToJSON Any

instance (ToJSON (f a)) => ToJSON (Alt f a)
instance (FromJSON (f a)) => FromJSON (Alt f a)

#if !MIN_VERSION_aeson(1,0,2)
instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
  parseJSON = parseJSONMap

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
  toJSON = toJSONMap
#endif

parseJSONMap :: (Ord k, FromJSON k, FromJSON v) => Value -> Parser (Map k v)
parseJSONMap v = Map.fromList <$> parseJSON v

toJSONMap :: (ToJSON k, ToJSON v) => Map k v -> Value
toJSONMap = toJSON . Map.toList
