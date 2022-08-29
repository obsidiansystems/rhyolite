{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Schema.NullableSubRow where

import Control.Lens
import Database.Beam

-- | Beamable table-like type with exactly one column
newtype NullableSubRow a f = NullableSubRow { unNullableSubRow :: (a (Nullable f)) } deriving Generic

_NullableSubRow :: Iso (NullableSubRow a f) (NullableSubRow b g) (a (Nullable f)) (b (Nullable g))
_NullableSubRow = iso unNullableSubRow NullableSubRow

deriving instance Eq (a (Nullable f)) => Eq (NullableSubRow a f)
deriving instance Ord (a (Nullable f)) => Ord (NullableSubRow a f)
deriving instance Show (a (Nullable f)) => Show (NullableSubRow a f)
deriving instance Num (a (Nullable f)) => Num (NullableSubRow a f)

instance Beamable a => Beamable (NullableSubRow a)
