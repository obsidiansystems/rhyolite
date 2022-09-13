{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Schema.NullableScalar where

import Control.Lens
import Database.Beam

-- | Beamable table-like type with exactly one column
newtype NullableScalar a f = NullableScalar { unNullableScalar :: (Columnar (Nullable f) a) } deriving Generic

_NullableScalar :: Iso (NullableScalar a f) (NullableScalar b g) (Columnar (Nullable f) a) (Columnar (Nullable g) b)
_NullableScalar = iso unNullableScalar NullableScalar

deriving instance Eq (Columnar (Nullable f) a) => Eq (NullableScalar a f)
deriving instance Ord (Columnar (Nullable f) a) => Ord (NullableScalar a f)
deriving instance Show (Columnar (Nullable f) a) => Show (NullableScalar a f)
deriving instance Num (Columnar (Nullable f) a) => Num (NullableScalar a f)

instance Beamable (NullableScalar a)
