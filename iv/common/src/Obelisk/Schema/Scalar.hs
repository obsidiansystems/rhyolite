{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Schema.Scalar where

import Control.Lens
import Database.Beam

-- | Beamable table-like type with exactly one column
newtype Scalar a f = Scalar { unScalar :: (Columnar f a) } deriving Generic

_Scalar :: Iso (Scalar a f) (Scalar b g) (Columnar f a) (Columnar g b)
_Scalar = iso unScalar Scalar

deriving instance Eq (Columnar f a) => Eq (Scalar a f)
deriving instance Ord (Columnar f a) => Ord (Scalar a f)
deriving instance Show (Columnar f a) => Show (Scalar a f)
deriving instance Num (Columnar f n) => Num (Scalar n f)

instance Beamable (Scalar a)
