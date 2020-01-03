-- | Some decoding functions integrated with "Aeson".

module Rhyolite.Request.Common where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as AesonParser
import qualified Data.Attoparsec.Lazy as LA
import qualified Data.ByteString.Lazy as LBS

decodeWith :: LA.Parser Aeson.Value -> (Aeson.Value -> Aeson.Result a) -> LBS.ByteString -> Maybe a
decodeWith p to s =
  case LA.parse p s of
    LA.Done _ v -> case to v of
      Aeson.Success a -> Just a
      _ -> Nothing
    _ -> Nothing

decodeValue' :: (Aeson.FromJSON a) => LBS.ByteString -> Maybe a
decodeValue' = decodeWith AesonParser.value' Aeson.fromJSON
