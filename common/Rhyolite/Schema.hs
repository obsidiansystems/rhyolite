-- | Part of the Rhyolite integration with
-- [groundhog](http://hackage.haskell.org/package/groundhog). In particular, we
-- define 'SchemaName' and 'LargeObjectId' here in order to send them across the
-- wire.

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rhyolite.Schema where

import Data.Aeson (FromJSON, ToJSON)
import Database.Id.Class
import Control.Category ((>>>))
import Control.Monad.Error (MonadError)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Obelisk.Route

newtype SchemaName = SchemaName { unSchemaName :: Text }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON, Typeable, Generic)

data WithSchema a = WithSchema SchemaName a
  deriving (Eq, Ord, Read, Show, Typeable, Generic)

withoutSchema :: WithSchema a -> a
withoutSchema (WithSchema _ a) = a

instance (FromJSON a) => FromJSON (WithSchema a)
instance (ToJSON a) => ToJSON (WithSchema a)

instance ShowPretty a => ShowPretty (IdValue a) where
  showPretty (IdValue _ x) = showPretty x

instance Show (IdData a) => ShowPretty (Id a) where
  showPretty = T.pack . show . unId

class ShowPretty a where
  showPretty :: a -> Text
  default showPretty :: Show a => a -> Text
  showPretty = T.pack . show

type Email = Text --TODO: Validation

-- | Wrapper for storing objects as JSON in the DB. Import the instance from
newtype Json a = Json { unJson :: a }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Newtype for referring to database large objects. This generally shouldn't have to go over the wire
-- but I'm putting it here where it can be placed in types in the common schema, because often the Ids of
-- those types will want to be shared with the frontend. We're using Word64 here rather than CUInt, which
-- is the type that Oid wraps, because Word64 has Groundhog instances to steal.
newtype LargeObjectId = LargeObjectId Word64
  deriving (Eq, Ord, Show, Read, Typeable)

idPathSegmentEncoder
  :: forall a check parse.
  (MonadError Text parse, Applicative check, Show (IdData a), Read (IdData a))
  => Encoder check parse (Id a) PageName
idPathSegmentEncoder = idEncoder >>> singlePathSegmentEncoder

idEncoder
  :: forall a check parse.
  (MonadError Text parse, Applicative check, Show (IdData a), Read (IdData a))
  => Encoder check parse (Id a) Text
idEncoder = unsafeMkEncoder EncoderImpl
  { _encoderImpl_encode = showPretty
  , _encoderImpl_decode = \x -> Id <$> tryDecode unsafeTshowEncoder x
  }
