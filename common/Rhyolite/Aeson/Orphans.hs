{-# Language CPP #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans #-}
module Rhyolite.Aeson.Orphans where

#if !MIN_VERSION_aeson(1,5,1) && MIN_VERSION_these(1,1,0)
-- The aeson instances for These are now in aeson instead of these.
-- reflex-platform hasn't updated to a newer aeson yet.

import Data.Aeson ((.=), FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson (pair)
import qualified Data.HashMap.Strict as HM
import Data.These

-- | @since 0.7.1
instance (ToJSON a, ToJSON b) => ToJSON (These a b) where
    toJSON (This a)    = Aeson.object [ "This" .= a ]
    toJSON (That b)    = Aeson.object [ "That" .= b ]
    toJSON (These a b) = Aeson.object [ "This" .= a, "That" .= b ]

    toEncoding (This a)    = Aeson.pairs $ "This" .= a
    toEncoding (That b)    = Aeson.pairs $ "That" .= b
    toEncoding (These a b) = Aeson.pairs $ "This" .= a <> "That" .= b

-- | @since 0.7.1
instance (FromJSON a, FromJSON b) => FromJSON (These a b) where
    parseJSON = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> parseJSON b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> parseJSON b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> parseJSON b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 0.7.2
instance Aeson.ToJSON2 These where
    liftToJSON2  toa _ _tob _ (This a)    = Aeson.object [ "This" .= toa a ]
    liftToJSON2 _toa _  tob _ (That b)    = Aeson.object [ "That" .= tob b ]
    liftToJSON2  toa _  tob _ (These a b) = Aeson.object [ "This" .= toa a, "That" .= tob b ]

    liftToEncoding2  toa _ _tob _ (This a)    = Aeson.pairs $ Aeson.pair "This" (toa a)
    liftToEncoding2 _toa _  tob _ (That b)    = Aeson.pairs $ Aeson.pair "That" (tob b)
    liftToEncoding2  toa _  tob _ (These a b) = Aeson.pairs $ Aeson.pair "This" (toa a) <> Aeson.pair "That" (tob b)

-- | @since 0.7.2
instance ToJSON a => Aeson.ToJSON1 (These a) where
    liftToJSON _tob _ (This a)    = Aeson.object [ "This" .= a ]
    liftToJSON  tob _ (That b)    = Aeson.object [ "That" .= tob b ]
    liftToJSON  tob _ (These a b) = Aeson.object [ "This" .= a, "That" .= tob b ]

    liftToEncoding _tob _ (This a)    = Aeson.pairs $ "This" .= a
    liftToEncoding  tob _ (That b)    = Aeson.pairs $ Aeson.pair "That" (tob b)
    liftToEncoding  tob _ (These a b) = Aeson.pairs $ "This" .= a <> Aeson.pair "That" (tob b)

-- | @since 0.7.2
instance Aeson.FromJSON2 These where
    liftParseJSON2 pa _ pb _ = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> pa a <*> pb b
        p [("That", b), ("This", a)] = These <$> pa a <*> pb b
        p [("This", a)] = This <$> pa a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

-- | @since 0.7.2
instance FromJSON a => Aeson.FromJSON1 (These a) where
    liftParseJSON pb _ = Aeson.withObject "These a b" (p . HM.toList)
      where
        p [("This", a), ("That", b)] = These <$> parseJSON a <*> pb b
        p [("That", b), ("This", a)] = These <$> parseJSON a <*> pb b
        p [("This", a)] = This <$> parseJSON a
        p [("That", b)] = That <$> pb b
        p _  = fail "Expected object with 'This' and 'That' keys only"

#else
#endif
