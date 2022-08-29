{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--TODO: Everything in this module should be upstreamed
module Obelisk.Reflex.Orphans () where

import Data.Aeson
import GHC.Generics hiding (R, C)
import Data.Patch
import Data.Patch.MapWithPatchingMove
import Data.Functor.Misc
import Data.Monoid

deriving instance (ToJSON k, ToJSONKey k, ToJSON p, ToJSON (PatchTarget p)) => ToJSON (PatchMapWithPatchingMove k p)
deriving instance (Ord k, FromJSON k, FromJSONKey k, FromJSON p, FromJSON (PatchTarget p)) => FromJSON (PatchMapWithPatchingMove k p)

deriving instance Generic (NodeInfo k p)
instance (ToJSON k, ToJSON p, ToJSON (PatchTarget p)) => ToJSON (NodeInfo k p)
instance (FromJSON k, FromJSON p, FromJSON (PatchTarget p)) => FromJSON (NodeInfo k p)

deriving instance Generic (From k p)
instance (ToJSON k, ToJSON p, ToJSON (PatchTarget p)) => ToJSON (From k p)
instance (FromJSON k, FromJSON p, FromJSON (PatchTarget p)) => FromJSON (From k p)

deriving instance FromJSON (f a) => FromJSON (ComposeMaybe f a)
deriving instance ToJSON (f a) => ToJSON (ComposeMaybe f a)

deriving instance Semigroup (Maybe (g a)) => Semigroup (ComposeMaybe g a)
deriving instance Monoid (Maybe (g a)) => Monoid (ComposeMaybe g a)

instance (Eq a, Num a) => Patch (Sum a) where
  type PatchTarget (Sum a) = a
  apply (Sum a) b = let s = a + b in
    if s == 0
    then Nothing
    else Just s
