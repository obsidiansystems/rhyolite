{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
module Obelisk.Beam.Schema
  ( module Obelisk.Beam.Schema
  , module X
  ) where

import Control.Lens
import Data.Aeson
import Database.Beam
import Database.Beam.Backend.SQL.Types
import GHC.Generics as X ((:+:)(..), (:*:)(..))
import Data.GADT.Compare

-- TODO upstream (or replace with something from upstream)
-- TODO: upstream has $Columnar'$
newtype Column a f = Column { unColumn :: Columnar f a }
  deriving (Generic)

instance Beamable (Column a)

deriving instance ToJSON (Columnar f a) => ToJSON (Column a f)
deriving instance FromJSON (Columnar f a) => FromJSON (Column a f)
deriving instance ToJSONKey (Columnar f a) => ToJSONKey (Column a f)
deriving instance FromJSONKey (Columnar f a) => FromJSONKey (Column a f)
deriving instance Ord (Columnar f a) => Ord (Column a f)
deriving instance Eq (Columnar f a) => Eq (Column a f)
deriving instance Show (Columnar f a) => Show (Column a f)

columnIso :: Iso (Column a f) (Column b g) (Columnar f a) (Columnar g b)
columnIso = iso unColumn Column

instance (Beamable f, Beamable g) => Beamable (f :*: g)

instance (Table f , Table g) => Table (f :*: g) where
  newtype PrimaryKey (f :*: g) h = CrossJoinId ((PrimaryKey f :*: PrimaryKey g) h) deriving Generic
  primaryKey (xs :*: ys) = CrossJoinId (primaryKey xs :*: primaryKey ys)

-- TODO: aeson-orphans
deriving instance (Eq (PrimaryKey f h), Eq (PrimaryKey g h)) => Eq (PrimaryKey (f :*: g) h)
deriving instance (Ord (PrimaryKey f h), Ord (PrimaryKey g h)) => Ord (PrimaryKey (f :*: g) h)
deriving instance (Show (PrimaryKey f h), Show (PrimaryKey g h)) => Show (PrimaryKey (f :*: g) h)

instance (Beamable (PrimaryKey f), Beamable (PrimaryKey g)) => Beamable (PrimaryKey (f :*: g))

instance (FromJSON (f a), FromJSON (g a)) => FromJSON ((f :*: g) a)
instance (FromJSON (f a), FromJSON (g a)) => FromJSONKey ((f :*: g) a)

instance (ToJSON (f a), ToJSON (g a)) => ToJSON ((f :*: g) a)
instance (ToJSON (f a), ToJSON (g a)) => ToJSONKey ((f :*: g) a)

deriving instance (FromJSON (PrimaryKey f h), FromJSON (PrimaryKey g h)) => FromJSON (PrimaryKey (f :*: g) h)
deriving instance (FromJSON (PrimaryKey f h), FromJSON (PrimaryKey g h)) => FromJSONKey (PrimaryKey (f :*: g) h)

deriving instance (ToJSON (PrimaryKey f h), ToJSON (PrimaryKey g h)) => ToJSON (PrimaryKey (f :*: g) h)
deriving instance (ToJSON (PrimaryKey f h), ToJSON (PrimaryKey g h)) => ToJSONKey (PrimaryKey (f :*: g) h)

_SqlSerial :: Iso (SqlSerial a) (SqlSerial b) a b
_SqlSerial = iso (\(SqlSerial a) -> a) SqlSerial
